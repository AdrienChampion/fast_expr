//! Generation of identifiers, types...

prelude! {}

use rust::Id;

pub fn span() -> rust::Span {
    rust::Span::mixed_site()
}

const FAST_EXPR_CRATE: &str = "fast_expr";

pub fn lib_path() -> Id {
    Id::new(FAST_EXPR_CRATE, gen::span())
}

pub mod fun {
    use super::*;

    pub fn zip(id: &rust::Id) -> rust::Id {
        let id = match rust::CamelId::try_from(id.clone()).and_then(|id| id.to_snake()) {
            Ok(id) => format!("zip_{}", id),
            Err(_) => format!("zip_{}", id),
        };
        Id::new(&id, span())
    }
    pub fn go_up(e_id: &rust::Id, v_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("go_up_{}_{}", e_id, v_id);
        Id::new(&id, span())
    }
    pub fn inspect(id: &rust::Id) -> rust::Id {
        let id = match rust::CamelId::try_from(id.clone()).and_then(|id| id.to_snake()) {
            Ok(id) => format!("inspect_{}", id),
            Err(_) => format!("inspect_{}", id),
        };
        Id::new(&id, span())
    }
    pub fn expr_handler(e_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let id = format!("handle_{}", e_id);
        Id::new(&id, span())
    }
    pub fn variant_handler(e_id: &rust::Id, v_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("handle_variant_{}_{}", e_id, v_id);
        Id::new(&id, span())
    }
    pub fn frame_handler(e_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let id = format!("handle_frame_{}", e_id);
        Id::new(&id, span())
    }

    pub fn initializer(e_id: &rust::Id, v_id: &rust::Id, d_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("coll_init_{}_{}_{}", e_id, v_id, d_id);
        Id::new(&id, span())
    }
    pub fn folder(e_id: &rust::Id, v_id: &rust::Id, d_id: &rust::Id) -> rust::Id {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("coll_fold_{}_{}_{}", e_id, v_id, d_id);
        Id::new(&id, span())
    }

    pub mod param {
        use super::*;

        pub fn data_param(id: Either<&rust::Id, impl Display>) -> rust::Id {
            let id = match id.map_left(|id| {
                (
                    id,
                    rust::CamelId::try_from(id.clone()).and_then(|id| id.to_snake()),
                )
            }) {
                Either::Left((_, Ok(id))) => id.to_string(),
                Either::Left((id, Err(_))) => id.to_string(),
                Either::Right(idx) => format!("elem_{}", idx),
            };
            Id::new(&id, span())
        }
    }
}

pub mod frame {
    use super::*;

    const SUFF: &str = "Frame";

    pub fn typ_id(id: impl Display) -> rust::Id {
        Id::new(&format!("{}{}", id, SUFF), span())
    }
    pub fn variant_id(
        e_variant: &rust::Id,
        d_idx: idx::Data,
        field: Option<&rust::Id>,
    ) -> rust::Id {
        let id = match field
            .map(|id| rust::SnakeId::try_from(id.clone()).and_then(|id| id.to_camel()))
        {
            Some(Ok(id)) => format!("{}{}", e_variant, id),
            None | Some(Err(_)) => format!("{}{}", e_variant, d_idx),
        };
        Id::new(&id, span())
    }

    pub fn sink_variant_id() -> rust::Id {
        Id::new("Sink", span())
    }
}

pub mod zip {
    use super::*;

    pub fn expr_var() -> rust::Id {
        Id::new("expr_var_reserved_for_fast_expr", span())
    }
    pub fn new_expr_var() -> rust::Id {
        Id::new("new_expr_var_reserved_for_fast_expr", span())
    }
    pub fn res_var() -> rust::Id {
        Id::new("res_var_reserved_for_fast_expr", span())
    }
    pub fn new_res_var() -> rust::Id {
        Id::new("new_res_var_reserved_for_fast_expr", span())
    }
    pub fn frame_var() -> rust::Id {
        Id::new("frame_var_reserved_for_fast_expr", span())
    }
    pub fn depth_var() -> rust::Id {
        Id::new("depth_var_reserved_for_fast_expr", span())
    }
    pub fn zip_do_var() -> rust::Id {
        Id::new("zip_do_var_reserved_for_fast_expr", span())
    }
}

pub mod lifetime {
    use super::*;

    pub fn expr() -> rust::Lifetime {
        rust::Lifetime {
            apostrophe: span(),
            ident: Id::new("fast_expr", span()),
        }
    }
}

pub mod typ {
    use super::*;

    pub fn res(id: impl Display) -> Id {
        Id::new(&format!("{}{}", id, "Res"), span())
    }
    pub fn acc(id: impl Display) -> Id {
        Id::new(&format!("{}{}", id, "Acc"), span())
    }
    pub fn zip(id: impl Display) -> Id {
        Id::new(&format!("{}{}", id, "Zip"), span())
    }

    pub mod param {
        use super::*;

        pub fn step() -> Id {
            Id::new("Step", span())
        }
        pub fn zip() -> Id {
            Id::new("Zip", span())
        }
    }
}

pub mod trai {
    use super::*;

    pub fn stepper(id: impl Display) -> Id {
        Id::new(&format!("{}{}", id, "Stepper"), span())
    }
    pub fn zipper(id: impl Display) -> Id {
        Id::new(&format!("{}{}", id, "Zipper"), span())
    }

    pub mod lib {
        use super::*;

        pub fn stepper() -> Id {
            Id::new("Stepper", span())
        }
        pub fn zipper() -> Id {
            Id::new("Zipper", span())
        }
        pub fn zipper_res_typ() -> Id {
            Id::new("Res", span())
        }
    }
}

pub mod field {
    use super::*;

    pub mod expr_zip {
        use super::*;

        pub fn stepper() -> Id {
            Id::new("step", span())
        }
        pub fn stack(id: Id) -> Id {
            let id = match rust::CamelId::try_from(id.clone()).and_then(|id| id.to_snake()) {
                Ok(id) => format!("{}_stack", id),
                Err(_) => format!("{}_stack", id),
            };
            Id::new(&id, span())
        }
        pub fn sink() -> Id {
            Id::new("_sink", span())
        }
    }
}

pub mod module {
    use super::*;

    pub fn zip_ref() -> Id {
        Id::new("zip_ref", span())
    }
    pub fn zip_own() -> Id {
        Id::new("zip_own", span())
    }
    pub fn zip(is_own: IsOwn) -> Id {
        if is_own {
            zip_own()
        } else {
            zip_ref()
        }
    }
}

pub mod punct {
    // use super::*;

    use syn::punctuated::Punctuated;

    pub fn do_with<Elm, Punct>(
        punct: &Punctuated<Elm, Punct>,
        mut action: impl FnMut(Option<&Punct>),
    ) {
        let mut punct = punct.pairs();
        'puncts: loop {
            use syn::punctuated::Pair::*;
            let punct = match punct.next() {
                Some(Punctuated(_, punct)) => Some(punct),
                Some(End(_)) => {
                    debug_assert!(punct.next().is_none());
                    None
                }
                None => break 'puncts,
            };
            action(punct)
        }
    }
}

pub mod doc {
    pub mod module {
        prelude! {}

        pub fn zip_ref() -> &'static str {
            "\
Zipper over expression references.\
            "
        }
        pub fn zip_own() -> &'static str {
            "\
Zipper over owned expressions.\
            "
        }
        pub fn zip(is_own: IsOwn) -> &'static str {
            if is_own {
                zip_own()
            } else {
                zip_ref()
            }
        }
    }
}

#[allow(non_upper_case_globals)]
pub mod lib {
    use super::*;

    pub fn path() -> Id {
        Id::new("fast_expr", span())
    }

    pub mod sink {
        use super::*;

        pub fn id() -> Id {
            Id::new("Sink", span())
        }

        pub fn instantiate(t: impl ToTokens) -> TokenStream {
            let path = gen::lib::path();
            let sink = id();
            quote! { #path :: #sink < #t > }
        }

        pub fn match_empty(id: &rust::Id) -> TokenStream {
            quote! { (_, #id) }
        }
    }

    pub mod empty {
        use super::*;

        pub fn id() -> Id {
            Id::new("Empty", span())
        }

        pub fn instantiate() -> TokenStream {
            let path = gen::lib::path();
            let empty = id();
            quote! { #path :: #empty }
        }
    }

    pub mod coll_der {
        use super::*;

        pub fn id() -> Id {
            Id::new("CollDer", span())
        }

        pub fn instantiate(acc: impl ToTokens, iter: impl ToTokens) -> TokenStream {
            let path = gen::lib::path();
            let coll_der = id();
            quote! {
                #path :: #coll_der < #acc, #iter >
            }
        }

        pub fn acc_field() -> Id {
            Id::new("acc", span())
        }
        pub fn iter_field() -> Id {
            Id::new("iter", span())
        }

        pub fn new(acc: impl ToTokens, iter: impl ToTokens) -> TokenStream {
            let path = gen::lib::path();
            let coll_der = id();
            quote! {
                #path :: #coll_der :: new (#acc, #iter)
            }
        }
    }

    pub mod zip_do {
        use super::*;

        pub fn id() -> Id {
            Id::new("ZipDo", span())
        }

        pub fn instantiate(
            down: impl ToTokens,
            expr: impl ToTokens,
            res: impl ToTokens,
        ) -> TokenStream {
            let path = gen::lib::path();
            let zip_do = id();
            quote! {
                #path :: #zip_do < #down, #expr, #res >
            }
        }

        pub fn go_down() -> Id {
            Id::new("GoDown", span())
        }
        pub fn go_up() -> Id {
            Id::new("GoUp", span())
        }
        pub fn subst() -> Id {
            Id::new("Subst", span())
        }
        pub fn early() -> Id {
            Id::new("Early", span())
        }

        pub fn match_cases(
            down_pat: impl ToTokens,
            down_do: impl ToTokens,
            up_pat: impl ToTokens,
            up_do: impl ToTokens,
            subst_pat: impl ToTokens,
            subst_do: impl ToTokens,
            early_pat: impl ToTokens,
            early_do: impl ToTokens,
        ) -> TokenStream {
            let path = gen::lib::path();
            let zip_do = id();
            let (down, up, subst, early) = (go_down(), go_up(), subst(), early());
            quote! {
                #path :: #zip_do :: #down (#down_pat) => #down_do,
                #path :: #zip_do :: #up (#up_pat) => #up_do,
                #path :: #zip_do :: #subst (#subst_pat) => #subst_do,
                #path :: #zip_do :: #early (#early_pat) => #early_do,
            }
        }

        pub fn map_down() -> Id {
            Id::new("map_down", span())
        }
        pub fn down_and_then() -> Id {
            Id::new("down_and_then", span())
        }

        fn new(variant: Id, inner: impl ToTokens) -> TokenStream {
            let lib_path = gen::lib::path();
            let zip_do = id();
            quote! {
                #lib_path :: #zip_do :: #variant ( #inner )
            }
        }

        pub fn new_go_down(inner: impl ToTokens) -> TokenStream {
            new(go_down(), inner)
        }
        pub fn new_go_up(inner: impl ToTokens) -> TokenStream {
            new(go_up(), inner)
        }
        pub fn new_subst(inner: impl ToTokens) -> TokenStream {
            new(subst(), inner)
        }
        pub fn new_early(inner: impl ToTokens) -> TokenStream {
            new(early(), inner)
        }

        pub fn early_return_if_not_down(expr: impl ToTokens) -> TokenStream {
            let path = gen::lib::path();
            let id = id();
            let go_down = go_down();
            let go_up = go_up();
            let subst = subst();
            let early = early();
            quote! {
                match #expr {
                    #path :: #id :: #go_down (stuff) => stuff,
                    #path :: #id :: #go_up (stuff) => return #path :: #id :: #go_up (stuff),
                    #path :: #id :: #subst (stuff) => return #path :: #id :: #subst (stuff),
                    #path :: #id :: #early (stuff) => return #path :: #id :: #early (stuff),
                }
            }
        }
    }

    pub mod zipper_trait {
        use super::*;

        pub fn id() -> Id {
            Id::new("Zipper", span())
        }
        pub fn res_t_param() -> Id {
            Id::new("Res", span())
        }
        pub fn zip_fn() -> Id {
            Id::new("zip", span())
        }
    }

    pub mod stepper_trait {
        use super::*;

        pub fn id() -> Id {
            Id::new("Stepper", span())
        }
        pub fn frame_t_param() -> Id {
            Id::new("Frame", span())
        }
        pub fn res_t_param() -> Id {
            Id::new("StepRes", span())
        }
        pub fn handle_expr_fn() -> Id {
            Id::new("handle_expr", span())
        }
        pub fn handle_frame_fn() -> Id {
            Id::new("handle_frame", span())
        }
    }
}
