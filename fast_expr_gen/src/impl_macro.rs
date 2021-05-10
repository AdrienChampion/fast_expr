prelude! {}

pub mod map;
pub mod zip;
pub mod impl_kw {
    syn::custom_keyword! { zip }
    syn::custom_keyword! { init }
    syn::custom_keyword! { step }
    syn::custom_keyword! { fold }
    syn::custom_keyword! { go_up }
    syn::custom_keyword! { map }
    syn::custom_keyword! { ref_trait }
    syn::custom_keyword! { own_trait }
}

/// Context for an impl-macro invocation.
#[derive(Debug, Clone)]
pub struct Instance {
    /// User-impl part.
    pub user_impl: UserImpl,
    /// User-zip, expression level.
    pub exprs: zip::Exprs,
    /// Receiver (`&mut self`).
    pub receiver: syn::Receiver,
    /// Expression info from the main fast-expr macro.
    pub expr_map: map::Exprs,
    /// Type definitions constructed from user input.
    ///
    /// Originally empty, filled in the last step before codegen.
    pub user_type_defs: Map<syn::Ident, TypeDef>,
    /// Function definitions constructor from user input.
    ///
    /// Originally empty, filled in the last step before codegen.
    pub user_fn_defs: Map<syn::Ident, FnDef>,
}

impl Instance {
    /// Checks its internal consistency.
    pub fn check(&self) -> Res<()> {
        self.exprs.check()?;
        Ok(())
    }

    /// Generates the tokens for the implementation requested by the user.
    pub fn generate_tokens(&mut self) -> Res<TokenStream> {
        self.check()?;

        let main_e_id = &self.exprs.main_e_id();
        let is_own = self.exprs.is_own();

        let main_e_map = self.expr_map.get(main_e_id)?;

        self.exprs.check_mentions_exactly(&main_e_map.deps)?;

        for expr in self.exprs.iter() {
            let e_map = self.expr_map.get(&expr.e_id)?;

            // Handle result type.
            let res_type = &expr.e_res_type;
            let res_type_id = &e_map.res_type_id;
            let _prev = self.user_type_defs.insert(
                res_type_id.clone(),
                TypeDef::new(vec![], res_type_id.clone(), res_type.clone()),
            );
            if _prev.is_some() {
                bail!(on res_type =>
                    "[internal] illegal redefinition of res-type `{}`",
                    res_type_id,
                )
            }

            for variant in &expr.variants {
                let v_map = e_map.get(&variant.v_id)?;
                let (type_defs, fn_defs) = (&mut self.user_type_defs, &mut self.user_fn_defs);
                variant.generate(
                    expr,
                    e_map,
                    v_map,
                    &self.receiver,
                    is_own,
                    |type_def| {
                        let span = type_def.def.span();
                        let prev = type_defs.insert(type_def.ident.clone(), type_def);
                        if let Some(type_def) = prev {
                            bail!(@span =>
                                "[internal] multiple definitions for type `{}`",
                                type_def.ident,
                            )
                        }
                        Ok(())
                    },
                    |fn_def| {
                        let span = fn_def.body.span();
                        let prev = fn_defs.insert(fn_def.sig.ident.clone(), fn_def);
                        if let Some(fn_def) = prev {
                            bail!(@span =>
                                "[internal] multiple definitions for method `{}`",
                                fn_def.sig.ident,
                            )
                        }
                        Ok(())
                    },
                )?
            }
        }

        let item_trait = main_e_map.get_trait(main_e_id.span(), is_own)?;

        let items = item_trait.items.iter().map(|item| match item {
            syn::TraitItem::Method(method) => {
                if method.default.is_some() {
                    // Default implementation provided, skipping.
                    quote! {}
                } else if let Some(fn_def) = self.user_fn_defs.get(&method.sig.ident) {
                    fn_def.to_token_stream()
                } else {
                    error!(on method =>
                        "[internal] unexpected zip-trait method {}",
                        method.to_token_stream(),
                    )
                    .to_compile_error()
                }
            }
            syn::TraitItem::Type(typ) => {
                if let Some(type_def) = self.user_type_defs.get(&typ.ident) {
                    type_def.to_token_stream()
                } else {
                    error!(on typ =>
                        "[internal] unexpected zip-trait associated type {}",
                        typ.to_token_stream(),
                    )
                    .to_compile_error()
                }
            }
            item => error!(on item =>
                "[internal] unexpected internal zip-trait item {}",
                item.to_token_stream(),
            )
            .to_compile_error(),
        });

        let UserImpl {
            impl_token,
            impl_trait,
            for_token,
            self_type,
            generics,
        } = &self.user_impl;

        let (t_params, _, where_clause) = generics.split_for_impl();
        Ok(quote! {
            #impl_token #t_params
            #impl_trait #for_token #self_type
            #where_clause {
                #(#items)*
            }
        })
    }
}

implement! {
    impl Instance {
        ToTokens, Display {
            |&self, tokens| {
                let Self {
                    expr_map,
                    user_impl,
                    receiver,
                    exprs,
                    user_type_defs,
                    user_fn_defs,
                } = self;

                let user_type_defs = user_type_defs.values();
                let user_fn_defs = user_fn_defs.values();

                tokens.extend(quote! {
                    #user_impl {
                        #receiver => {
                            #exprs
                        }
                    }
                    #expr_map

                    user_type_defs {
                        #(#user_type_defs)*
                    }
                    user_fn_defs {
                        #(#user_fn_defs)*
                    }
                })
            }
        }

        Parse {
            |input| {
                let user_impl = input.parse()?;

                let (receiver, exprs_content) = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);
                    let receiver = brace_content.parse()?;
                    let _: syn::Token![=>] = brace_content.parse()?;
                    (receiver, brace_content)
                };

                let exprs = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in exprs_content);
                    brace_content.parse()?
                };

                let expr_map = input.parse()?;

                Ok(Self {
                    expr_map,
                    user_impl,
                    receiver,
                    exprs,
                    user_type_defs: Map::new(),
                    user_fn_defs: Map::new(),
                })
            }
        }
    }
}

/// User-impl part of the impl-macro invocation.
#[derive(Debug, Clone)]
pub struct UserImpl {
    /// Impl token.
    pub impl_token: syn::Token![impl],
    /// Trait being implemented.
    pub impl_trait: syn::Path,
    /// For token.
    pub for_token: syn::Token![for],
    /// Self-type.
    pub self_type: syn::Type,
    /// Generics of the implementation.
    pub generics: syn::Generics,
}

implement! {
    impl UserImpl {
        ToTokens, Display {
            |&self, tokens| {
                let Self { impl_token, impl_trait, for_token, self_type, generics } = self;
                let (tparams, _, where_clause) = generics.split_for_impl();
                tokens.extend(quote! {
                    #impl_token #tparams
                    #impl_trait #for_token #self_type
                    #where_clause
                })
            }
        }

        Parse {
            |input| {
                let impl_token = input.parse()?;
                let mut generics: syn::Generics = input.parse()?;
                if let Some(where_clause) = &generics.where_clause {
                    bail!(on where_clause => "unexpected where clause")
                }
                let impl_trait = input.parse()?;
                let for_token = input.parse()?;
                let self_type = input.parse()?;
                let where_clause = input.parse()?;
                generics.where_clause = where_clause;

                Ok(Self { impl_token, impl_trait, for_token, self_type, generics })
            }
        }
    }
}

/// Associated type definition.
///
/// Definition comes from the user, ident is retrieved by the impl-macro.
#[derive(Debug, Clone)]
pub struct TypeDef {
    /// Attributes.
    pub attrs: rust::Attributes,
    /// Associated type identifier.
    pub ident: syn::Ident,
    /// Type definition (from user).
    pub def: syn::Type,
}
impl TypeDef {
    /// Constructor.
    pub fn new(attrs: rust::Attributes, ident: impl Into<syn::Ident>, def: syn::Type) -> Self {
        Self {
            attrs,
            ident: ident.into(),
            def,
        }
    }
}

implement! {
    impl TypeDef {
        ToTokens, Display {
            |&self, tokens| {
                let Self { attrs, ident, def } = self;
                let span = def.span();
                let def = quote::quote_spanned! {
                    span=> type #ident = #def ;
                };
                tokens.extend(quote! {
                    #(#attrs)*
                    #def
                })
            }
        }
    }
}

/// Function definition.
///
/// A definition is an identifier, some bindings for the inputs, and a possibly empty list of
/// redefinitions.
#[derive(Debug, Clone)]
pub struct FnDef {
    /// Span corresponding to this function definition, for error-reporting.
    pub span: Span,
    /// Attributes.
    pub attrs: Vec<syn::Attribute>,
    /// Function signature.
    pub sig: syn::Signature,
    /// Redefinitions.
    pub redefs: Vec<Redef>,
    /// Function body.
    pub body: syn::Expr,
}

impl FnDef {
    /// Constructor.
    pub fn new(
        span: Span,
        attrs: Vec<syn::Attribute>,
        sig: syn::Signature,
        redefs: Vec<Redef>,
        body: syn::Expr,
    ) -> Self {
        Self {
            span,
            attrs,
            sig,
            redefs,
            body,
        }
    }

    /// Adds documentation attributes if none are present.
    pub fn add_doc_if_none(&mut self, doc: impl IntoIterator<Item = syn::Attribute>) {
        let doc_path: syn::Path = syn::parse_quote!(doc);
        if self
            .attrs
            .iter()
            .all(|attribute| attribute.path != doc_path)
        {
            self.attrs.extend(doc)
        }
    }
}

implement! {
    impl FnDef {
        ToTokens, Display {
            |&self, tokens| {
                let Self { span, attrs, sig, redefs, body } = self;
                tokens.extend(quote_spanned! { *span=>
                    #(#attrs)*
                    #sig {
                        #(#redefs)*
                        #body
                    }
                })
            }
        }
    }
}

/// Redefinition.
///
/// Used in function definitions to rewrite inputs with user bindings.
#[derive(Debug, Clone)]
pub struct Redef {
    /// Entity to redefine.
    pub src: TokenStream,
    /// Redefinition target.
    pub tgt: TokenStream,
}
impl Redef {
    /// Constructor.
    pub fn new(src: TokenStream, tgt: TokenStream) -> Self {
        Self { src, tgt }
    }
}

implement! {
    impl Redef {
        ToTokens, Display {
            |&self, tokens| {
                let Self { src, tgt } = self;
                tokens.extend(quote! {
                    let #tgt = #src;
                })
            }
        }
    }
}
