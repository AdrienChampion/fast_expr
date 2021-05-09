//! Collection-like data handling.

prelude! {}

#[derive(Debug, Clone)]
struct CollSpec {
    pub typ: idx::TypPath,
    pub ref_iter: idx::TypPath,
    pub own_iter: idx::TypPath,
}
impl CollSpec {
    pub fn typ_id(&self) -> StaticTypId {
        self.typ.get_path().id
    }
    pub fn typ(&self) -> idx::TypPath {
        self.typ
    }
    pub fn iter(&self, is_own: IsOwn) -> idx::TypPath {
        if is_own {
            self.own_iter
        } else {
            self.ref_iter
        }
    }

    pub fn iter_on(&self, is_own: IsOwn, typ: &rust::Typ) -> rust::Typ {
        let arg = syn::parse_quote!(Item = #typ);
        self.iter(is_own)
            .get_path()
            .to_typ(gen::span(), Some(vec![arg]))
    }

    pub fn iter_fun(&self, is_own: IsOwn) -> rust::Id {
        if is_own {
            rust::Id::new("into_iter", gen::span())
        } else {
            rust::Id::new("iter", gen::span())
        }
    }
}

macro_rules! coll_spec {
    (
        $($field:ident : $val:expr),* $(,)?
    ) => {
        CollSpec {
            $($field: $val),*
        }
    };
}

static VEC: CollSpec = coll_spec! {
    typ: builtin::path::vec_path(),
    ref_iter: builtin::path::vec_ref_iter_path(),
    own_iter: builtin::path::vec_own_iter_path(),
};
static HASH_SET: CollSpec = coll_spec! {
    typ: builtin::path::hash_set_path(),
    ref_iter: builtin::path::hash_set_ref_iter_path(),
    own_iter: builtin::path::hash_set_own_iter_path(),
};
static BTREE_SET: CollSpec = coll_spec! {
    typ: builtin::path::btree_set_path(),
    ref_iter: builtin::path::btree_set_ref_iter_path(),
    own_iter: builtin::path::btree_set_own_iter_path(),
};

#[derive(Debug, Clone, Copy)]
pub enum Coll {
    Vec,
    HashSet,
    BTreeSet,
}
impl Coll {
    pub const PREF: &'static str = "coll";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == VEC.typ_id() {
            Ok(Self::Vec)
        } else if id == HASH_SET.typ_id() {
            Ok(Self::HashSet)
        } else if id == BTREE_SET.typ_id() {
            Ok(Self::BTreeSet)
        } else {
            bail!(on(id, "unknown collection type"))
        }
    }

    fn spec(self) -> &'static CollSpec {
        match self {
            Self::Vec => &VEC,
            Self::HashSet => &HASH_SET,
            Self::BTreeSet => &BTREE_SET,
        }
    }
    pub fn wrap(self, coll_span: rust::Span, inner: rust::Typ) -> rust::Typ {
        self.spec()
            .typ()
            .get_path()
            .to_typ(coll_span, Some(vec![rust::GenericArg::Type(inner)]))
    }
}

#[derive(Debug, Clone)]
pub struct Many {
    coll_span: rust::Span,
    coll: Coll,
    inner: super::One,
    typ: rust::Typ,
    c_idx: idx::Coll,
}
impl Many {
    pub fn new(
        _cxt: &cxt::PreCxt,
        coll_span: rust::Span,
        coll: Coll,
        inner: super::One,
        c_idx: idx::Coll,
    ) -> Res<Self> {
        if !inner.is_plain() {
            bail!(@(coll_span, "only collections of plain (not wrapped) expressions are supported"))
        }
        let typ = coll.wrap(coll_span, inner.typ().clone());
        Ok(Self {
            coll_span,
            coll,
            inner,
            typ,
            c_idx,
        })
    }

    pub fn map_rec_exprs(&self, action: impl FnMut(idx::Expr) -> Res<()>) -> Res<()> {
        self.inner.map_rec_exprs(action)
    }

    pub fn inner(&self) -> idx::Expr {
        self.inner.inner()
    }
    pub fn e_idx(&self) -> idx::Expr {
        self.inner.e_idx()
    }
    pub fn v_idx(&self) -> idx::Variant {
        self.inner.v_idx()
    }
    pub fn d_idx(&self) -> idx::Data {
        self.inner.d_idx()
    }
    pub fn c_idx(&self) -> idx::Coll {
        self.c_idx
    }

    pub fn acc_t_param<'a>(&self, cxt: &'a impl cxt::PreCxtLike) -> &'a rust::Typ {
        cxt.get_pre_e_cxt(self.e_idx()).colls()[self.c_idx].acc_t_param()
    }

    pub fn iter_typ(&self, is_own: IsOwn, typ: &rust::Typ) -> rust::Typ {
        self.coll.spec().iter_on(is_own, typ)
    }
    pub fn iter_fun(&self, is_own: IsOwn) -> rust::Id {
        self.coll.spec().iter_fun(is_own)
    }

    pub fn is_self_rec(&self) -> bool {
        self.inner.is_self_rec()
    }
}

impl Many {
    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }

    #[inline]
    pub fn needs_frame(&self) -> bool {
        true
    }

    pub fn frame_typ(&self, _cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        let typ = self.typ.clone();
        if is_own {
            typ
        } else {
            rust::typ::to_expr_ref(typ)
        }
    }
    pub fn frame_der(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> Option<rust::Typ> {
        let spec = self.coll.spec();
        let acc = self.acc_t_param(cxt);

        let mut args = vec![];
        if !is_own {
            args.push(rust::typ::generic_arg::from_lifetime(gen::lifetime::expr()))
        }
        args.push(rust::typ::generic_arg::from_typ(self.inner.typ().clone()));

        let iter = spec
            .iter(is_own)
            .get_path()
            .to_typ(rust::Span::mixed_site(), Some(args));
        let typ_tokens = cxt.lib_gen().coll_der_instantiate(acc, &iter);
        Some(syn::parse_quote!(#typ_tokens))
    }
    pub fn frame_res(&self, cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> rust::Typ {
        self.acc_t_param(cxt).clone()
    }
    pub fn zip_res(&self, cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> rust::Typ {
        let acc_t_param = self.acc_t_param(cxt);
        syn::parse_quote!(Self :: #acc_t_param)
    }
}

// impl Many {
//     pub fn extract_expr(
//         &self,
//         slf: &rust::Id,
//         is_own: IsOwn,
//         if_some: impl FnOnce(TokenStream) -> TokenStream,
//         if_none: impl FnOnce() -> TokenStream,
//     ) -> TokenStream {
//         match &self.wrap {
//             Wrap::Plain => if_some(quote!(#slf)),
//             Wrap::Box(_) => if_some(if is_own {
//                 quote!(* #slf)
//             } else {
//                 quote!(&** #slf)
//             }),
//             Wrap::Ref { .. } => {
//                 assert!(!is_own);
//                 if_some(quote!(&** #slf))
//             }
//             Wrap::Option(_) => {
//                 let as_ref = if is_own { quote!() } else { quote!(.as_ref()) };
//                 let if_some = if_some(slf.to_token_stream());
//                 let if_none = if_none();
//                 quote! {
//                     if let Some(#slf) = #slf #as_ref {
//                         #if_some
//                     } else {
//                         let #slf = None;
//                         #if_none
//                     }
//                 }
//             }
//         }
//     }
// }
