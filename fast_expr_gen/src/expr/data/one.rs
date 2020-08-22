//! Single expression data handling.

prelude! {}

static BOX: StaticTypPath = static_typ_path! {
    pref: &["std", "boxed"],
    id: "Box",
};
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
    Option(rust::Span),
}
impl Wrap {
    pub const PREF: &'static str = "wrap";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == BOX.id {
            Ok(Wrap::Box(id.span()))
        } else if id == OPTION.id {
            Ok(Wrap::Option(id.span()))
        } else {
            bail!(on(id, "unknown wrapper type"))
        }
    }

    pub fn wrap(&self, inner: rust::Typ) -> rust::Typ {
        match self {
            Self::Plain => inner,
            Self::Box(span) => BOX.to_typ(*span, Some(vec![rust::GenericArg::Type(inner)])),
            Self::Option(span) => OPTION.to_typ(*span, Some(vec![rust::GenericArg::Type(inner)])),
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
