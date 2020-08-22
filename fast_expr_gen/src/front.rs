//! Frontend representation of an expression type and its sub-expressions.

use syn::parse::{Parse, ParseStream};

prelude! {}

/// Keywords of the fast-expr DSL.
pub mod keyword {
    syn::custom_keyword! {
        // Keyword indicating the start of a spec trait.
        spec
    }
}

/// Structure that comes out of the very first parsing step.
pub struct Top {
    /// Specifications.
    pub specs: Vec<rust::Trait>,
    /// Expression types.
    pub exprs: Vec<rust::Enum>,
}

impl Top {
    /// Empty constructor.
    fn new() -> Self {
        Self {
            specs: vec![],
            exprs: vec![],
        }
    }

    /// Parses a token stream.
    fn from_stream(&mut self, input: ParseStream) -> Res<()> {
        while !input.is_empty() {
            // Not sure if we're parsing a spec or an expression, get the attributes first.
            let mut attrs = input.call(rust::Attribute::parse_outer)?;

            // Now that we have the attributes, let's look ahead to decide what to parse.
            let lookahead = input.lookahead1();

            if lookahead.peek(keyword::spec) {
                // Parsing a spec.
                let _: keyword::spec = input.parse()?;
                let mut spec: rust::Trait = input.parse()?;
                // Put the attributes in there.
                attrs = std::mem::replace(&mut spec.attrs, attrs);
                spec.attrs.extend(attrs);
                // Done, update `self`.
                self.specs.push(spec)
            } else if lookahead.peek(syn::Token![pub]) || lookahead.peek(syn::Token![enum]) {
                // Parsing an expression.
                let mut expr: rust::Enum = input.parse()?;
                // Put the attributes in there.
                attrs = std::mem::replace(&mut expr.attrs, attrs);
                expr.attrs.extend(attrs);
                // Done, update `self`.
                self.exprs.push(expr)
            } else {
                return Err(lookahead.error());
            }
        }

        Ok(())
    }
}

impl Parse for Top {
    fn parse(input: ParseStream) -> Res<Self> {
        let mut slf = Self::new();
        slf.from_stream(input)?;
        Ok(slf)
    }
}
