//! Code generation crate for fast-expr.

pub extern crate quote;
pub extern crate syn;

#[macro_use]
pub mod macros;

pub mod check;
pub mod cxt;
pub mod err;
pub mod expr;
pub mod front;
pub mod gen;
pub mod prelude;
pub mod rust;

prelude! {}

/// Procedural macro entry-point, yields a top context for zipper generation.
pub fn generate_context(top: front::Top) -> Res<cxt::ZipTop> {
    cxt::Top::new(top).map(|top| {
        top.dbg_log_to_file();
        top
    })
}

/// Parses a stream of tokens.
///
/// This is used in tests to check the result of a `quote!` call.
pub fn from_stream(stream: TokenStream) -> Res<TokenStream> {
    let top = syn::parse_quote!(#stream);
    generate_context(top).map(|top| top.to_token_stream())
}
