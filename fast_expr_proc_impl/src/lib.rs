//! Zip-spec-trait implementation procedural macro.
//!
//! This macro is a helper for end-user to implement zip-spec-traits.

extern crate proc_macro;

fast_expr_gen::prelude! {}

use fast_expr_gen::{impl_macro::Instance, syn};

/// Helper for zip-spec-trait implementation.
///
/// This macro is not meant to be used directly. The main fast_expr procedural macro will generate
/// custom helper macros for your ADTs which will rely on this procedural macro.
#[proc_macro]
pub fn fast_expr_impl(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut instance = syn::parse_macro_input!(stream as Instance);

    let tokens = match instance.generate_tokens() {
        Ok(tokens) => {
            quote! {
                #tokens
                // pub static OUTPUT: &str = stringify!(#tokens);
            }
        }
        Err(e) => e.to_compile_error(),
    };
    proc_macro::TokenStream::from(tokens)
}
