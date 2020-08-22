//! Generation of identifiers, types...

prelude! {}

use rust::Id;

fn span() -> rust::Span {
    rust::Span::mixed_site()
}

pub mod frame {
    use super::*;

    const SUFF: &str = "Frame";

    pub fn typ_id(id: impl Display) -> rust::Id {
        Id::new(&format!("{}{}", id, SUFF), span())
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
