//! Generation of identifiers, types...

prelude! {}

pub mod frame {
    use super::*;

    const SUFF: &str = "Frame";

    pub fn typ_id(id: &rust::Id) -> rust::Id {
        rust::Id::new(&format!("{}{}", id, SUFF), id.span())
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
