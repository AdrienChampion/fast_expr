//! Fast expr derive.

extern crate proc_macro;

use fast_expr_gen::{log, logln};

pub(crate) use fast_expr_gen::{quote, syn};

mod ast;

/// Entry point, parses a token stream and generates code.
#[proc_macro]
pub fn expr(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    logln!("parsing...");

    let _expr_def = syn::parse_macro_input!(stream as ast::Expr);

    log!({
        logln!("done parsing, top expr is {} {{", _expr_def.top.ident);
        for variant in &_expr_def.top.variants {
            use syn::Fields::*;
            match &variant.fields {
                Named(fields) => {
                    let mut s = String::new();
                    s.push_str("{{ ");
                    for field in &fields.named {
                        if let Some(ident) = &field.ident {
                            s.push_str(&ident.to_string());
                            s.push_str(": ");
                        }
                        {
                            use quote::ToTokens;
                            s.push_str(&field.ty.to_token_stream().to_string());
                        }
                        s.push_str(", ");
                    }
                    s.push_str("}}");
                    logln!("    {} {},", variant.ident, s)
                }
                Unnamed(fields) => {
                    let mut s = String::new();
                    s.push_str("( ");
                    for field in &fields.unnamed {
                        if let Some(ident) = &field.ident {
                            s.push_str(&ident.to_string());
                            s.push_str(": ");
                        }
                        {
                            use quote::ToTokens;
                            s.push_str(&field.ty.to_token_stream().to_string());
                        }
                        s.push_str(", ");
                    }
                    s.push_str(")");
                    logln!("    {} {},", variant.ident, s)
                }
                Unit => logln!("    {},", variant.ident),
            }
        }
        logln!("}}");
    });

    logln!(
        "building code-gen structures for `{}`...",
        _expr_def.top.ident
    );

    "".parse().unwrap()
}
