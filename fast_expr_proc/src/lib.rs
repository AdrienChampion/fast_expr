//! Fast expr derive.

extern crate proc_macro;

fast_expr_gen::prelude! {}

use fast_expr_gen::{quote, syn};

/// Entry point, parses a token stream and generates code.
#[proc_macro]
pub fn expr(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    logln!("parsing...");

    let expr_def = syn::parse_macro_input!(stream as front::Expr);

    log!({
        logln!("done parsing, top expr is {} {{", expr_def.top.ident);
        for variant in &expr_def.top.variants {
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
                        let blah = match &field.ty {
                            syn::Type::Path(path) => format!("{:?}", path),
                            _ => "not a path".into(),
                        };
                        s.push_str(&format!(" ({})", blah));
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

    logln!();

    match internal(expr_def) {
        Ok(res) => res,
        Err(e) => {
            logln!("code generation failed...");
            logln!();

            proc_macro::TokenStream::from(e.to_compile_error())
        }
    }
}

fn internal(expr: front::Expr) -> Result<proc_macro::TokenStream> {
    fast_expr_gen::generate_context(expr)?;

    Ok("".parse().unwrap())
}
