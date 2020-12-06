//! Operations over types.

use super::*;
use syn::punctuated::Punctuated;

fn new_segment(ident: Ident, args: Option<GenericArgs>) -> syn::PathSegment {
    let arguments = {
        if let Some(gen_args) = args {
            let mut args = Punctuated::new();
            for arg in gen_args {
                args.push(arg)
            }
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: rust::token::Lt {
                    spans: [Span::mixed_site()],
                },
                args,
                gt_token: rust::token::Gt {
                    spans: [Span::mixed_site()],
                },
            })
        } else {
            syn::PathArguments::None
        }
    };

    syn::PathSegment { ident, arguments }
}

fn typ_from_segments(segments: Punctuated<syn::PathSegment, rust::token::Colon2>) -> Type {
    Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments,
        },
    })
}

pub fn plain(ident: Ident, args: Option<GenericArgs>) -> Type {
    let segments = {
        let mut segments = Punctuated::new();
        segments.push(new_segment(ident, args));
        segments
    };
    typ_from_segments(segments)
}

pub fn tuple(typs: impl IntoIterator<Item = Type>) -> Type {
    Type::Tuple(syn::TypeTuple {
        paren_token: rust::token::Paren::default(),
        elems: typs.into_iter().collect(),
    })
}

fn internal_ref(lifetime: Option<Lifetime>, typ: Type, is_mut: bool) -> Type {
    Type::Reference(syn::TypeReference {
        and_token: rust::token::And::default(),
        lifetime,
        mutability: if is_mut {
            Some(rust::token::Mut::default())
        } else {
            None
        },
        elem: Box::new(typ),
    })
}
pub fn reference(lifetime: Option<Lifetime>, typ: Type) -> Type {
    internal_ref(lifetime, typ, false)
}
pub fn reference_if(cond: bool, lifetime: Option<Lifetime>, typ: Type) -> Type {
    if cond {
        reference(lifetime, typ)
    } else {
        typ
    }
}
pub fn reference_mut(lifetime: Option<Lifetime>, typ: Type) -> Type {
    internal_ref(lifetime, typ, true)
}
pub fn reference_mut_if(cond: bool, lifetime: Option<Lifetime>, typ: Type) -> Type {
    if cond {
        reference_mut(lifetime, typ)
    } else {
        typ
    }
}

pub fn simple_path(
    path: impl IntoIterator<Item = Ident>,
    ident: Ident,
    args: Option<GenericArgs>,
) -> Type {
    let segments = {
        let mut segments = Punctuated::new();
        for seg in path {
            segments.push(new_segment(seg, None));
        }
        segments.push(new_segment(ident, args));
        segments
    };
    typ_from_segments(segments)
}

pub mod param {
    use super::*;

    pub fn from_id(ident: Ident) -> TypParam {
        TypParam {
            attrs: vec![],
            ident,
            colon_token: None,
            bounds: syn::punctuated::Punctuated::new(),
            eq_token: None,
            default: None,
        }
    }
}

pub mod generic_param {
    use super::*;

    pub fn from_id(id: Ident) -> GenericParam {
        GenericParam::Type(param::from_id(id))
    }
    pub fn from_lifetime(lifetime: rust::Lifetime) -> GenericParam {
        GenericParam::Lifetime(syn::LifetimeDef {
            attrs: vec![],
            lifetime,
            colon_token: None,
            bounds: syn::punctuated::Punctuated::new(),
        })
    }
}

pub mod generic_arg {
    use super::*;

    pub fn from_typ(typ: Type) -> GenericArg {
        GenericArg::Type(typ)
    }
    pub fn from_lifetime(lt: Lifetime) -> GenericArg {
        GenericArg::Lifetime(lt)
    }
}

pub mod lib {
    use super::*;

    pub fn empty() -> Type {
        let path = Some(gen::lib_path());
        let id = Ident::new("Empty", gen::span());
        rust::typ::simple_path(path, id, None)
    }

    pub fn zipper(e_typ: Type) -> Type {
        let path = Some(gen::lib_path());
        simple_path(
            path,
            gen::trai::lib::zipper(),
            Some(vec![generic_arg::from_typ(e_typ)]),
        )
    }
    pub fn stepper(e_typ: Type) -> Type {
        let path = Some(gen::lib_path());
        simple_path(
            path,
            gen::trai::lib::stepper(),
            Some(vec![generic_arg::from_typ(e_typ)]),
        )
    }

    pub fn zip_do(down_typ: Type, e_typ: Type, res_typ: Type) -> Type {
        let path = Some(gen::lib_path());
        let id = Ident::new("ZipDo", gen::span());
        simple_path(
            path,
            id,
            Some(vec![
                generic_arg::from_typ(down_typ),
                generic_arg::from_typ(e_typ),
                generic_arg::from_typ(res_typ),
            ]),
        )
    }
}

pub fn to_expr_ref(typ: Type) -> Type {
    Type::Reference(syn::TypeReference {
        and_token: rust::token::And {
            spans: [gen::span()],
        },
        lifetime: Some(gen::lifetime::expr()),
        mutability: None,
        elem: Box::new(typ),
    })
}
pub fn to_expr_ref_if(cond: bool, typ: Type) -> Type {
    if cond {
        Type::Reference(syn::TypeReference {
            and_token: rust::token::And {
                spans: [gen::span()],
            },
            lifetime: Some(gen::lifetime::expr()),
            mutability: None,
            elem: Box::new(typ),
        })
    } else {
        typ
    }
}
