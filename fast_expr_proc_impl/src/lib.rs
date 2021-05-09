//! Zip-spec-trait implementation procedural macro.
//!
//! This macro is a helper for end-user to implement zip-spec-traits.

extern crate proc_macro;

use std::collections::BTreeMap as Map;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn::{
    self,
    parse::{Parse, ParseStream},
};

mod expr_map;
mod expr_zip;

mod keyword {
    use super::*;

    syn::custom_keyword! { zip }
    syn::custom_keyword! { init }
    syn::custom_keyword! { step }
    syn::custom_keyword! { fold }
    syn::custom_keyword! { go_up }
    syn::custom_keyword! { map }
}

/// Helper for zip-spec-trait implementation.
///
/// This macro is not meant to be used directly. The main fast_expr procedural macro will generate
/// custom helper macros for your ADTs which will rely on this procedural macro.
#[proc_macro]
pub fn fast_expr_impl(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut instance = syn::parse_macro_input!(stream as Instance);

    let tokens = match instance.work() {
        Ok(()) => {
            // let dbg_tokens = instance.to_dbg_tokens();
            let impl_tokens = instance.to_impl_tokens();
            quote! {
                #impl_tokens
                // pub static OUTPUT: &str = stringify!(#impl_tokens);
            }
        }
        Err(e) => e.to_compile_error(),
    };
    proc_macro::TokenStream::from(tokens)
}

struct Instance {
    pub zip_trait: rust::Trait,
    pub expr_map: expr_map::ExprMap,
    pub impl_token: syn::token::Impl,
    pub impl_trait: syn::Path,
    pub impl_for: syn::token::For,
    pub impl_self_type: syn::Type,
    pub impl_generics: syn::Generics,
    pub impl_brace: syn::token::Brace,
    pub receiver: syn::Receiver,
    pub exprs: Vec<expr_zip::ExprZip>,
    pub user_typ_map: Map<rust::Id, TokenStream>,
    pub user_fn_map: Map<
        rust::Id,
        (
            Vec<syn::Attribute>,
            Vec<TokenStream>,
            Vec<(TokenStream, TokenStream)>,
            rust::Expr,
        ),
    >,
}

impl Parse for Instance {
    fn parse(input: ParseStream) -> Res<Self> {
        let mut zip_trait = rust::Trait::parse(input)?;
        let expr_map = input.parse()?;

        let impl_token = input.parse()?;
        let mut impl_generics: syn::Generics = input.parse()?;
        if let Some(where_clause) = &impl_generics.where_clause {
            return Err(syn::Error::new_spanned(
                where_clause,
                "unexpected where clause",
            ));
        }
        let impl_trait = input.parse()?;
        let impl_for = input.parse()?;
        let impl_self_type = input.parse()?;
        let where_clause: Option<syn::WhereClause> = input.parse()?;
        impl_generics.where_clause = where_clause;

        let brace_content;
        let _ = syn::braced!(brace_content in input);

        let receiver = brace_content.parse()?;
        let _: syn::Token![=>] = brace_content.parse()?;
        let brace_content_2;
        let impl_brace = syn::braced!(brace_content_2 in brace_content);
        let mut exprs = vec![];
        while !brace_content_2.is_empty() {
            exprs.push(brace_content_2.parse()?)
        }

        if !brace_content.is_empty() {
            return Err(brace_content.error("expected EOI"));
        }
        if !input.is_empty() {
            return Err(input.error("expected EOI"));
        }

        // Filter out methods that already have an implementation (default).
        zip_trait.items = zip_trait
            .items
            .into_iter()
            .filter(|item| {
                if let syn::TraitItem::Method(method) = item {
                    method.default.is_none()
                } else {
                    true
                }
            })
            .collect();

        Ok(Self {
            zip_trait,
            expr_map,
            impl_token,
            impl_trait,
            impl_for,
            impl_self_type,
            impl_generics,
            impl_brace,
            receiver,
            exprs,
            user_typ_map: Map::new(),
            user_fn_map: Map::new(),
        })
    }
}

impl Instance {
    #[allow(dead_code)]
    fn to_dbg_tokens(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.zip_trait.to_tokens(&mut tokens);
        self.expr_map.to_tokens(&mut tokens);
        let user_typ_map = self
            .user_typ_map
            .iter()
            .map(|(id, def)| quote! { #id => #def });
        let user_fn_map = self
            .user_fn_map
            .iter()
            .map(|(id, (attrs, args, redefs, def))| {
                if redefs.is_empty() {
                    quote! {
                        #(#attrs)*
                        fn #id ( #(#args),* ) {
                            #def
                        }
                    }
                } else {
                    let redefs = redefs.iter().map(|(src, tgt)| quote! { #tgt <- #src });
                    quote! {
                        #(#attrs)*
                        fn #id( #(#args),* where #(#redefs),* ) {
                            #def
                        }
                    }
                }
            });
        tokens.extend(quote! {
            {
                user_typ_map {
                    #(#user_typ_map),*
                }
                user_fn_map {
                    #(#user_fn_map),*
                }
            }
        });

        tokens
    }

    fn to_impl_tokens(&self) -> TokenStream {
        let impl_items = self.zip_trait.items.iter().map(|item| match item {
            syn::TraitItem::Method(method) => {
                let attrs = &method.attrs;
                let mut sig = method.sig.clone();
                assert_eq!(None, method.default);
                match self.user_fn_map.get(&sig.ident) {
                    None => syn::Error::new_spanned(&sig.ident, "unknown zip-spec function")
                        .to_compile_error(),
                    Some((_, user_inputs, _, _)) if sig.inputs.len() != user_inputs.len() + 1 => {
                        syn::Error::new_spanned(
                            &sig.ident,
                            format!("unexpected arity, expected {}", user_inputs.len() + 1),
                        )
                        .to_compile_error()
                    }
                    Some((user_attrs, user_inputs, user_redefs, user_def)) => {
                        debug_assert_eq!(sig.inputs.len(), user_inputs.len() + 1);
                        for (idx, arg) in sig.inputs.iter_mut().enumerate() {
                            match arg {
                                syn::FnArg::Receiver(receiver) => {
                                    if idx != 0 {
                                        return syn::Error::new_spanned(
                                            arg,
                                            format!("unexpected receiver in position {}", idx),
                                        )
                                        .to_compile_error();
                                    }
                                    *receiver = self.receiver.clone();
                                }
                                syn::FnArg::Typed(pat_type) => {
                                    if idx < 1 {
                                        return syn::Error::new_spanned(
                                            arg,
                                            format!("unexpected typed input in position {}", idx),
                                        )
                                        .to_compile_error();
                                    }
                                    let pat = &user_inputs[idx - 1];
                                    pat_type.pat = Box::new(syn::Pat::Verbatim(pat.clone()))
                                }
                            }
                        }
                        let user_redefs = user_redefs.iter().map(|(src, tgt)| {
                            quote! {
                                let #tgt = #src;
                            }
                        });

                        let attrs = if user_attrs.is_empty() {
                            attrs
                        } else {
                            user_attrs
                        };

                        quote! {
                            #(#attrs)*
                            #sig {
                                #(#user_redefs)*
                                #user_def
                            }
                        }
                    }
                }
            }
            syn::TraitItem::Type(type_item) => {
                let mut type_item = type_item.clone();
                assert_eq!(type_item.default, None);
                if let Some(user_type) = self.user_typ_map.get(&type_item.ident) {
                    type_item.default = Some((
                        syn::Token![=](Span::call_site()),
                        syn::Type::Verbatim(user_type.clone()),
                    ));
                    type_item.to_token_stream()
                } else {
                    syn::Error::new_spanned(&type_item.ident, "unexpected associated type")
                        .to_compile_error()
                }
            }
            item => {
                syn::Error::new_spanned(item, "[internal] unexpected trait item").to_compile_error()
            }
        });

        let Self {
            impl_token,
            impl_trait,
            impl_for,
            impl_self_type,
            impl_generics,
            ..
        } = self;
        let (impl_generics, _, impl_where_clause) = impl_generics.split_for_impl();
        quote! {
            #impl_token #impl_generics #impl_trait #impl_for #impl_self_type #impl_where_clause {
                #(#impl_items)*
            }
        }
    }
}

impl ToTokens for Instance {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.to_impl_tokens())
    }
}

impl Instance {
    fn work(&mut self) -> Res<()> {
        for expr_zip in &self.exprs {
            let expr_val = self.expr_map.get(&expr_zip.e_id)?;

            // Result type.
            let res_typ = &expr_zip.res_typ;
            let _prev = self
                .user_typ_map
                .insert(expr_val.res_typ_id.clone(), res_typ.to_token_stream());
            if _prev.is_some() {
                panic!("[internal] non-empty user type map")
            }

            for variant_zip in &expr_zip.variants {
                let v_id = variant_zip.v_id();
                let variant_val = expr_val.get(v_id)?;

                let go_up_id = &variant_val.go_up_id;
                let (args, redefs): (Vec<_>, Vec<_>) = variant_zip.bindings_do(
                    |pats| {
                        let (mut args, mut redefs) = (vec![], vec![]);

                        for field_pat in pats {
                            let d_id = match &field_pat.member {
                                syn::Member::Named(d_id) => d_id,
                                syn::Member::Unnamed(idx) => {
                                    return Err(syn::Error::new_spanned(
                                        idx,
                                        "expected identifier",
                                    ));
                                }
                            };
                            args.push(d_id.to_token_stream());
                            if field_pat.colon_token.is_some() {
                                redefs
                                    .push((d_id.to_token_stream(), field_pat.pat.to_token_stream()))
                            }
                        }

                        Ok((args, redefs))
                    },
                    |pats| {
                        Ok((
                            pats.iter().map(|pat| pat.to_token_stream()).collect(),
                            vec![],
                        ))
                    },
                )?;

                let (attrs, def) = variant_zip.go_up_def();

                let _prev = self.user_fn_map.insert(
                    go_up_id.clone(),
                    (
                        attrs.cloned().unwrap_or_else(Vec::new),
                        args.clone(),
                        redefs,
                        def.clone(),
                    ),
                );
                if _prev.is_some() {
                    return Err(syn::Error::new_spanned(
                        v_id,
                        format!("trying to zip variant `{}` twice", v_id.to_token_stream()),
                    ));
                }

                if let Some(multi) = variant_zip.get_multi() {
                    for fold in multi.folds() {
                        use expr_map::CollData;
                        // Resolve coll map.
                        let data = &fold.field;
                        let data_id: CollData = variant_zip
                            .bindings_do(
                                |sfields| {
                                    Ok(sfields
                                        .iter()
                                        .fold(None, |opt, field| {
                                            if opt.is_some() {
                                                opt
                                            } else {
                                                match &field.member {
                                                    syn::Member::Named(id) if id == data => {
                                                        Some(id)
                                                    }
                                                    _ => None,
                                                }
                                            }
                                        })
                                        .map(|id| CollData::Field(id.clone())))
                                },
                                |tfields| {
                                    Ok(tfields
                                        .iter()
                                        .enumerate()
                                        .fold(None, |opt, (idx, pat)| {
                                            if opt.is_some() {
                                                opt
                                            } else {
                                                match pat {
                                                    syn::Pat::Ident(id) if &id.ident == data => {
                                                        Some(syn::LitInt::new(
                                                            &format!("{}usize", idx),
                                                            id.ident.span(),
                                                        ))
                                                    }
                                                    _ => None,
                                                }
                                            }
                                        })
                                        .map(CollData::Idx))
                                },
                            )?
                            .ok_or_else(|| syn::Error::new_spanned(data, "unknown data ID"))?;

                        let acc_typ = fold.target.as_ref().map(|pair| &pair.1).unwrap_or(res_typ);

                        let mut coll_desc = None;
                        for coll in variant_val.colls.iter() {
                            if coll.data == data_id {
                                coll_desc = Some(coll);
                                break;
                            } else {
                                continue;
                            }
                        }
                        let coll_desc = coll_desc.ok_or_else(|| {
                            syn::Error::new_spanned(data, "not a collection data ID")
                        })?;

                        // acc type
                        {
                            let acc_typ_id = &coll_desc.acc_typ;
                            let _prev = self
                                .user_typ_map
                                .insert(acc_typ_id.clone(), acc_typ.to_token_stream());
                            if _prev.is_some() {
                                panic!("[internal] redefinition of acc-type `{}`", acc_typ_id)
                            }
                        }

                        // init
                        {
                            let args = args.clone();
                            let mut attrs = fold.attrs.clone();
                            attrs.extend(fold.init.attrs.clone());
                            let def = &fold.init.expr;
                            let _prev = self.user_fn_map.insert(
                                coll_desc.init_fn.clone(),
                                (attrs, args.clone(), vec![], def.clone()),
                            );
                            if _prev.is_some() {
                                return Err(syn::Error::new_spanned(
                                    v_id,
                                    format!(
                                        "trying to init data `{}` for variant `{}` twice",
                                        data, v_id,
                                    ),
                                ));
                            }
                        }

                        // fold
                        {
                            let args = args.clone();
                            let mut attrs = fold.attrs.clone();
                            attrs.extend(fold.step.attrs.clone());
                            let def = &fold.step.expr;
                            let (acc_pat, next_pat) = (&fold.step.acc_pat, &fold.step.next_pat);
                            let redefs =
                                vec![(data.to_token_stream(), quote! { (#acc_pat, #next_pat) })];
                            let _prev = self.user_fn_map.insert(
                                coll_desc.fold_fn.clone(),
                                (attrs, args.clone(), redefs, def.clone()),
                            );
                            if _prev.is_some() {
                                return Err(syn::Error::new_spanned(
                                    v_id,
                                    format!(
                                        "trying to fold data `{}` for variant `{}` twice",
                                        data, v_id,
                                    ),
                                ));
                            }
                        }

                        // panic!(
                        //     "acc_typ: {}, data_id: {:?}",
                        //     acc_typ.to_token_stream(),
                        //     data_id
                        // )
                    }
                }
            }
        }
        Ok(())
    }
}
