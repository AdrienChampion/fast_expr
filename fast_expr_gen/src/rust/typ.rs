//! Operations over types.

use super::*;
use syn::punctuated::Punctuated;

fn new_segment(ident: Id, args: Option<GenericArgs>) -> syn::PathSegment {
    let arguments = {
        if let Some(gen_args) = args {
            let mut args = Punctuated::new();
            for arg in gen_args {
                args.push(arg)
            }
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: syn::token::Lt {
                    spans: [Span::mixed_site()],
                },
                args,
                gt_token: syn::token::Gt {
                    spans: [Span::mixed_site()],
                },
            })
        } else {
            syn::PathArguments::None
        }
    };

    syn::PathSegment { ident, arguments }
}

fn typ_from_segments(segments: Punctuated<syn::PathSegment, syn::token::Colon2>) -> Typ {
    Typ::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments,
        },
    })
}

pub fn plain(ident: Id, args: Option<GenericArgs>) -> Typ {
    let segments = {
        let mut segments = Punctuated::new();
        segments.push(new_segment(ident, args));
        segments
    };
    typ_from_segments(segments)
}

pub fn tuple(typs: impl IntoIterator<Item = Typ>) -> Typ {
    Typ::Tuple(syn::TypeTuple {
        paren_token: syn::token::Paren::default(),
        elems: typs.into_iter().collect(),
    })
}

fn internal_ref(lifetime: Option<Lifetime>, typ: Typ, is_mut: bool) -> Typ {
    Typ::Reference(syn::TypeReference {
        and_token: syn::token::And::default(),
        lifetime,
        mutability: if is_mut {
            Some(syn::token::Mut::default())
        } else {
            None
        },
        elem: Box::new(typ),
    })
}
pub fn reference(lifetime: Option<Lifetime>, typ: Typ) -> Typ {
    internal_ref(lifetime, typ, false)
}
pub fn reference_mut(lifetime: Option<Lifetime>, typ: Typ) -> Typ {
    internal_ref(lifetime, typ, true)
}

pub fn simple_path(
    path: impl IntoIterator<Item = Id>,
    ident: Id,
    args: Option<GenericArgs>,
) -> Typ {
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

    pub fn from_id(ident: Id) -> TypParam {
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

    pub fn from_id(id: Id) -> GenericParam {
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

    pub fn from_typ(typ: Typ) -> GenericArg {
        GenericArg::Type(typ)
    }
    pub fn from_lifetime(lt: Lifetime) -> GenericArg {
        GenericArg::Lifetime(lt)
    }
}

pub mod lib {
    use super::*;

    pub fn sink(typ: Typ) -> Typ {
        let path = Some(gen::lib_path());
        let id = Id::new("Sink", gen::span());
        rust::typ::simple_path(path, id, Some(vec![generic_arg::from_typ(typ)]))
    }

    pub fn empty() -> rust::Typ {
        let path = Some(gen::lib_path());
        let id = Id::new("Empty", gen::span());
        rust::typ::simple_path(path, id, None)
    }

    pub fn coll_der(acc: &rust::Typ, iter: &rust::Typ) -> rust::Typ {
        let coll_der = gen::lib::coll_der::instantiate(acc, iter);
        syn::parse_quote!(#coll_der)
    }

    pub fn zipper(e_typ: rust::Typ) -> rust::Typ {
        let path = Some(gen::lib_path());
        simple_path(
            path,
            gen::trai::lib::zipper(),
            Some(vec![generic_arg::from_typ(e_typ)]),
        )
    }
    pub fn stepper(e_typ: rust::Typ) -> rust::Typ {
        let path = Some(gen::lib_path());
        simple_path(
            path,
            gen::trai::lib::stepper(),
            Some(vec![generic_arg::from_typ(e_typ)]),
        )
    }

    pub fn zip_do(down_typ: rust::Typ, e_typ: rust::Typ, res_typ: rust::Typ) -> rust::Typ {
        let path = Some(gen::lib_path());
        let id = Id::new("ZipDo", gen::span());
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

pub fn to_expr_ref(typ: rust::Typ) -> Typ {
    Typ::Reference(syn::TypeReference {
        and_token: syn::token::And {
            spans: [gen::span()],
        },
        lifetime: Some(gen::lifetime::expr()),
        mutability: None,
        elem: Box::new(typ),
    })
}
