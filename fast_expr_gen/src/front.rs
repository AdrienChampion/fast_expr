//! Frontend representation of an expression type and its sub-expressions.

use syn::{
    parse::{Nothing, Parse, ParseStream, Result},
    punctuated::Punctuated,
    token::Brace,
    ItemEnum,
};

/// Type alias for a repetition of something separated by nothing.
pub type List<T> = Punctuated<T, Nothing>;

/// Keywords of the fast-expr DSL.
pub mod keyword {
    syn::custom_keyword! {
        // Keyword indicating the list of sub-expression types.
        subs
    }
}

/// Structure that comes out of the very first parsing step.
pub struct Expr {
    /// Top expression enumeration.
    pub top: ItemEnum,
    /// Brace span of the sub-expression block.
    pub subs_brace: Brace,
    /// Sub-expression types (enums).
    pub subs: List<ItemEnum>,
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let top = input.parse()?;

        let _: keyword::subs = input.parse()?;

        let def;
        let subs_brace = syn::braced!(def in input);
        let subs = def.parse_terminated(ItemEnum::parse)?;

        Ok(Self {
            top,
            subs_brace,
            subs,
        })
    }
}
