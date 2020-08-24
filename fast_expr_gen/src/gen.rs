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

pub mod lifetime {
    use super::*;

    pub fn expr() -> rust::Lifetime {
        rust::Lifetime {
            apostrophe: span(),
            ident: Id::new("_unique_fast_expr_lifetime", span()),
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

    pub mod lib {
        use super::*;

        pub fn stepper() -> Id {
            Id::new("Stepper", span())
        }
        pub fn zipper() -> Id {
            Id::new("Zipper", span())
        }
    }
}

pub mod field {
    use super::*;

    pub mod expr_zip {
        use super::*;

        pub fn zipper() -> Id {
            Id::new("zip", span())
        }
        pub fn stack(id: Id) -> Id {
            let id = match rust::SnakeId::try_from(id.clone()).and_then(|id| id.to_camel()) {
                Ok(id) => format!("{}_stack", id),
                Err(_) => format!("{}_stack", id),
            };
            Id::new(&id, span())
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
