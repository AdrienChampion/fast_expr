//! Frontend representation of an expression type and its sub-expressions.

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

prelude! {}

/// Keywords of the fast-expr DSL.
pub mod keyword {
    syn::custom_keyword! {
        // Keyword indicating the start of a spec trait.
        spec
    }
    syn::custom_keyword! {
        // Keyword indicating the start of a fast_expr conf attribute.
        fast_expr
    }
}

/// Structure that comes out of the very first parsing step.
pub struct Top {
    /// Fast_expr configuration.
    pub conf: conf::Conf,
    /// Specifications.
    pub specs: Vec<rust::Trait>,
    /// Expression types.
    pub exprs: Vec<(conf::EConf, rust::Enum)>,
}

impl Top {
    /// Empty constructor.
    fn new(conf: conf::Conf) -> Self {
        Self {
            conf,
            specs: vec![],
            exprs: vec![],
        }
    }
}

impl Parse for Top {
    fn parse(input: ParseStream) -> Res<Self> {
        let inner_attrs: Vec<rust::Attribute> = rust::Attribute::parse_inner(input)?;
        let conf = conf::Conf::new(inner_attrs)?;

        let mut slf = Self::new(conf);

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
                slf.specs.push(spec)
            } else if lookahead.peek(syn::Token![pub]) || lookahead.peek(syn::Token![enum]) {
                // Parsing an expression.
                let mut expr: rust::Enum = input.parse()?;
                // Put the attributes in there.
                attrs = std::mem::replace(&mut expr.attrs, attrs);
                expr.attrs.extend(attrs);
                let e_conf = conf::EConf::new(&slf.conf, &mut expr)?;
                // Done, update `self`.
                slf.exprs.push((e_conf, expr));
            } else {
                return Err(lookahead.error());
            }
        }

        Ok(slf)
    }
}

pub enum LitOrId {
    Lit(rust::Lit),
    Id(rust::Id),
}
impl LitOrId {
    pub fn span(&self) -> rust::Span {
        match self {
            Self::Lit(lit) => lit.span(),
            Self::Id(id) => id.span(),
        }
    }
}
impl Parse for LitOrId {
    fn parse(input: ParseStream) -> Res<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(rust::Id) {
            let id = input.parse()?;
            Ok(Self::Id(id))
        } else if lookahead.peek(rust::Lit) {
            let lit = input.parse()?;
            Ok(Self::Lit(lit))
        } else {
            return Err(lookahead.error());
        }
    }
}

pub struct Conf {
    pub fields: Punctuated<ConfField, syn::token::Comma>,
}
impl Conf {
    pub fn fast_expr_attr_key() -> syn::Path {
        syn::parse_quote! { fast_expr }
    }
}
impl Parse for Conf {
    fn parse(input: ParseStream) -> Res<Self> {
        let content;
        syn::parenthesized!(content in input);
        let fields = Punctuated::parse_terminated(&content)?;

        Ok(Self { fields })
    }
}

pub struct ConfField {
    pub id: rust::Id,
    pub val: Option<(syn::token::Eq, LitOrId)>,
}
impl ConfField {
    pub fn into_bool(self) -> Res<(rust::Span, bool)> {
        let val = match self.val {
            None => true,
            Some((_, LitOrId::Lit(rust::Lit::Bool(b)))) => b.value,
            Some((_, val)) => bail!(@(val.span(), "expected boolean or nothing (meaning `true`)")),
        };
        Ok((self.id.span(), val))
    }

    pub fn into_id(self) -> Res<(rust::Span, rust::Id)> {
        match self.val {
            None => bail!(on(self.id, "expected identifier")),
            Some((_, LitOrId::Id(id))) => Ok((id.span(), id)),
            Some((_, val)) => bail!(@(val.span(), "expected identifier")),
        }
    }
}
impl Parse for ConfField {
    fn parse(input: ParseStream) -> Res<Self> {
        let id = input.parse()?;

        let val = if input.peek(syn::token::Comma) {
            None
        } else {
            let colon = input.parse()?;
            let lit = input.parse()?;
            Some((colon, lit))
        };

        Ok(Self { id, val })
    }
}
