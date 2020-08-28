//! Fast expr derive.

extern crate proc_macro;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn;

/// Entry point, parses a token stream and generates code.
#[proc_macro]
pub fn expr(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let top = syn::parse_macro_input!(stream as front::Top);

    let res = fast_expr_gen::generate_context(top)
        .map(|top| proc_macro::TokenStream::from(top.to_token_stream()));

    match res {
        Ok(res) => res,
        Err(e) => {
            logln!("code generation failed...");
            logln!();

            proc_macro::TokenStream::from(e.to_compile_error())
        }
    }
}
