//! Single expression data handling.

prelude! {}

static BOX: StaticTypPath = static_typ_path! {
    pref: &["std", "boxed"],
    id: "Box",
};

#[allow(dead_code)]
static OPTION: StaticTypPath = static_typ_path! {
    pref: &["std", "option"],
    id: "Option",
};

#[derive(Debug, Clone)]
pub enum Wrap {
    Plain,
    Box(rust::Span),
    Ref {
        and_token: syn::token::And,
        lifetime: rust::Lifetime,
    },
    // Option(rust::Span),
}
impl Wrap {
    pub const PREF: &'static str = "wrap";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == BOX.id {
            Ok(Wrap::Box(id.span()))
        // } else if id == OPTION.id {
        //     Ok(Wrap::Option(id.span()))
        } else {
            bail!(on(id, "unknown wrapper type"))
        }
    }

    pub fn wrap(&self, inner: rust::Typ) -> rust::Typ {
        match self {
            Self::Plain => inner,
            Self::Box(span) => BOX.to_typ(*span, Some(vec![rust::GenericArg::Type(inner)])),
            // Self::Option(span) => OPTION.to_typ(*span, Some(vec![rust::GenericArg::Type(inner)])),
            Self::Ref {
                and_token,
                lifetime,
            } => rust::Typ::Reference(syn::TypeReference {
                and_token: and_token.clone(),
                lifetime: Some(lifetime.clone()),
                mutability: None,
                elem: Box::new(inner),
            }),
        }
    }

    pub fn is_plain(&self) -> bool {
        if let Self::Plain = self {
            true
        } else {
            false
        }
    }
}

/// Represents a variant that stores one type of self-expression.
#[derive(Debug, Clone)]
pub struct One {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,
    inner: idx::Expr,
    id: rust::Id,
    args: Option<rust::GenericArgs>,
    typ: rust::Typ,
    res_typ: rust::Typ,
    e_typ: rust::Typ,
    wrap: Wrap,
}
impl One {
    /// Constructor.
    pub fn new_self(
        cxt: &cxt::PreCxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        slf: rust::Id,
        wrap: Wrap,
    ) -> Self {
        debug_assert_eq!(slf, "Self");
        let args = Some(cxt[e_idx].top_t_params().clone());
        let e_typ = {
            let mut id = cxt[e_idx].e_id().clone();
            id.set_span(slf.span());
            rust::typ::plain(id, args.clone())
        };
        let typ = wrap.wrap(e_typ.clone());
        let res_typ = cxt[e_idx].res_typ().clone();
        Self {
            e_idx,
            v_idx,
            d_idx,
            inner: e_idx,
            id: slf,
            args: None,
            typ,
            e_typ,
            res_typ,
            wrap,
        }
    }

    /// Constructor.
    pub fn new(
        cxt: &cxt::PreCxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        inner: idx::Expr,
        args: rust::GenericArgs,
        wrap: Wrap,
    ) -> Self {
        let id = cxt[inner].e_id().clone();
        let args = Some(args);
        let e_typ = rust::typ::plain(id.clone(), args.clone());
        let typ = wrap.wrap(e_typ.clone());
        let res_typ = cxt[inner].res_typ().clone();
        Self {
            e_idx,
            v_idx,
            d_idx,
            inner,
            id,
            args,
            typ,
            res_typ,
            e_typ,
            wrap,
        }
    }

    pub fn map_rec_exprs(&self, mut action: impl FnMut(idx::Expr) -> Res<()>) -> Res<()> {
        action(self.inner)
    }

    pub fn inner(&self) -> idx::Expr {
        self.inner
    }
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }
    pub fn v_idx(&self) -> idx::Variant {
        self.v_idx
    }
    pub fn d_idx(&self) -> idx::Data {
        self.d_idx
    }

    pub fn is_self_rec(&self) -> bool {
        self.e_idx == self.inner
    }
    pub fn is_plain(&self) -> bool {
        self.wrap.is_plain()
    }
}

impl One {
    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }

    #[inline]
    pub fn needs_frame(&self) -> bool {
        self.is_self_rec()
    }
    pub fn frame_typ(&self, _cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        let typ = self.typ.clone();
        if is_own {
            typ
        } else {
            rust::typ::to_expr_ref(typ)
        }
    }
    pub fn frame_der(&self, _cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> Option<rust::Typ> {
        None
    }
    pub fn frame_res(&self, _cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> rust::Typ {
        self.res_typ.clone()
    }
    pub fn zip_res(&self, _cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> rust::Typ {
        let res = &self.res_typ;
        syn::parse_quote!(Self :: #res)
    }
}

impl One {
    pub fn extract_expr(
        &self,
        slf: &rust::Id,
        is_own: IsOwn,
        if_some: impl FnOnce(TokenStream) -> TokenStream,
        // if_none: impl FnOnce() -> TokenStream,
    ) -> TokenStream {
        match &self.wrap {
            Wrap::Plain => if_some(quote!(#slf)),
            Wrap::Box(_) => if_some(if is_own {
                quote!(* #slf)
            } else {
                quote!(&** #slf)
            }),
            Wrap::Ref { .. } => {
                assert!(!is_own);
                if_some(quote!(&** #slf))
            } // Wrap::Option(_) => {
              //     let as_ref = if is_own { quote!() } else { quote!(.as_ref()) };
              //     let if_some = if_some(slf.to_token_stream());
              //     let if_none = if_none();
              //     quote! {
              //         if let Some(#slf) = #slf #as_ref {
              //             #if_some
              //         } else {
              //             let #slf = None;
              //             #if_none
              //         }
              //     }
              // }
        }
    }
}
