//! Collection-like data handling.

prelude! {}

#[derive(Debug, Clone)]
struct CollSpec {
    pub typ: StaticTypPath,
    pub ref_iter: StaticTypPath,
    pub own_iter: StaticTypPath,
}
impl CollSpec {
    pub fn typ_id(&self) -> StaticTypId {
        self.typ.id
    }
    pub fn typ(&self) -> StaticTypPath {
        self.typ
    }
    pub fn iter(&self, is_own: IsOwn) -> StaticTypPath {
        if is_own {
            self.own_iter
        } else {
            self.ref_iter
        }
    }

    pub fn iter_on(&self, is_own: IsOwn, typ: &rust::Typ) -> rust::Typ {
        let arg = syn::parse_quote!(Item = #typ);
        self.iter(is_own).to_typ(gen::span(), Some(vec![arg]))
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
        $($field:ident : ($pref:expr, $id:expr)),* $(,)?
    ) => {
        CollSpec {
            $($field: static_typ_path! {
                pref: $pref,
                id: $id,
            }),*
        }
    };
}

static COLL_PATH: [&'static str; 2] = ["std", "collection"];
static BTREE_SET_PATH: [&'static str; 3] = [COLL_PATH[0], COLL_PATH[1], "btree_set"];
static HASH_SET_PATH: [&'static str; 3] = [COLL_PATH[0], COLL_PATH[1], "hash_set"];
static VEC_PATH: [&'static str; 2] = ["std", "vec"];
static SLICE_PATH: [&'static str; 2] = ["std", "slice"];

static VEC: CollSpec = coll_spec! {
    typ: (&VEC_PATH, "Vec"),
    ref_iter: (&SLICE_PATH, "Iter"),
    own_iter: (&VEC_PATH, "IntoIter"),
};
static HASH_SET: CollSpec = coll_spec! {
    typ: (&COLL_PATH, "HashSet"),
    ref_iter: (&HASH_SET_PATH, "Iter"),
    own_iter: (&COLL_PATH, "IntoIter"),
};
static BTREE_SET: CollSpec = coll_spec! {
    typ: (&COLL_PATH, "BTreeSet"),
    ref_iter: (&BTREE_SET_PATH, "Iter"),
    own_iter: (&BTREE_SET_PATH, "IntoIter"),
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

    pub fn acc_t_param<'a>(&self, e_cxt: &'a cxt::pre::ECxt) -> &'a rust::Typ {
        e_cxt.colls()[self.c_idx].acc_t_param()
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

    pub fn frame_typ(&self, _e_cxt: &cxt::pre::ECxt, is_own: IsOwn) -> rust::Typ {
        let typ = self.typ.clone();
        if is_own {
            typ
        } else {
            rust::typ::to_expr_ref(typ)
        }
    }
    pub fn frame_der(&self, e_cxt: &cxt::pre::ECxt, is_own: IsOwn) -> Option<rust::Typ> {
        let spec = self.coll.spec();
        let acc = self.acc_t_param(e_cxt);

        let mut args = vec![];
        if !is_own {
            args.push(rust::typ::generic_arg::from_lifetime(gen::lifetime::expr()))
        }
        args.push(rust::typ::generic_arg::from_typ(self.inner.typ().clone()));

        let iter = spec
            .iter(is_own)
            .to_typ(rust::Span::mixed_site(), Some(args));
        Some(rust::typ::lib::coll_der(acc, &iter))
    }
    pub fn frame_res(&self, e_cxt: &cxt::pre::ECxt, _is_own: IsOwn) -> rust::Typ {
        self.acc_t_param(e_cxt).clone()
    }
    pub fn zip_res(&self, e_cxt: &cxt::pre::ECxt, _is_own: IsOwn) -> rust::Typ {
        let acc_t_param = self.acc_t_param(e_cxt);
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
