//! Types for the part of the impl-macro input coming from fast-expr.

prelude! {}

use super::impl_kw as kw;

/// Info for all expressions.
#[derive(Debug, Clone)]
pub struct Exprs {
    /// Map keyword.
    pub key: kw::map,
    /// Maps expression idents to expression infos.
    pub map: Map<syn::Ident, Expr>,
}
impl Exprs {
    /// Constructor from a zip context.
    pub fn new(cxt: &cxt::ZipCxt) -> Self {
        let key = kw::map(Span::call_site());

        let mut map = Map::new();
        for e_cxt in cxt.e_cxts() {
            let expr = Expr::new(e_cxt, cxt);
            let e_id = expr.e_id.clone();
            let _prev = map.insert(e_id, expr);
            if let Some(expr) = _prev {
                panic!(
                    "[internal] trying to register map for expression `{}` twice",
                    expr.e_id,
                )
            }
        }

        Self { key, map }
    }

    /// Retrieves a map for an expression.
    pub fn get(&self, expr_id: &syn::Ident) -> Res<&Expr> {
        self.map.get(expr_id).ok_or_else(|| {
            error!(on expr_id =>
                "unknown expression type identifier `{}`", expr_id
            )
        })
    }
}

implement! {
    impl Exprs {
        ToTokens, Display {
            |&self, tokens| {
                let Self { key, map } = self;
                let map = map.values();
                tokens.extend(quote! {
                    #key { #(#map),* }
                })
            }
        }

        Parse {
            |input| {
                let key = input.parse()?;

                let exprs = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);
                    Csv::<Expr>::parse_terminated(&brace_content)?
                };

                let mut map = Map::new();
                for expr in exprs {
                    let span = expr.e_id.span();
                    let prev = map.insert(expr.e_id.clone(), expr);
                    if let Some(expr) = prev {
                        bail!(@span =>
                            "[internal] expression `{}` appears twice in fast-expr map",
                            expr.e_id,
                        )
                    }
                }

                Ok(Self { key, map })
            }
        }
    }
}

/// Expression info.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Expression identifier.
    pub e_id: syn::Ident,
    /// Result type identifier.
    pub res_type_id: syn::Ident,
    /// Map from variant identifiers to variant info.
    pub map: Map<syn::Ident, Variant>,
    /// Ref-trait associated with this expression.
    pub ref_trait: Option<syn::ItemTrait>,
    /// Own-trait associated with this expression.
    pub own_trait: Option<syn::ItemTrait>,
    /// Other expressions this expression depends on.
    ///
    /// Includes itself.
    pub deps: Set<syn::Ident>,
}
impl Expr {
    /// Constructor from a zip context.
    pub fn new(e_cxt: &cxt::zip::ECxt, cxt: &cxt::ZipCxt) -> Self {
        let (ref_gen, own_gen) = (cxt.conf().ref_gen.get(), cxt.conf().own_gen.get());
        let e_id = e_cxt.e_id().clone();
        let res_type_id = e_cxt.res_typ_id().clone();

        let mut map = Map::new();
        for variant in e_cxt.expr().variants() {
            let variant = Variant::new(variant, cxt);
            let v_id = variant.v_id.clone();
            let _prev = map.insert(v_id.clone(), variant);
            if _prev.is_some() {
                panic!(
                    "[internal] trying to construct map for variant `{}::{}` twice",
                    e_id, v_id
                )
            }
        }

        let ref_trait: Option<syn::ItemTrait> = if ref_gen {
            let tokens = e_cxt.zip_trait_tokens(cxt, false);
            Some(syn::parse_quote!(#tokens))
        } else {
            None
        };
        let own_trait = if own_gen {
            let tokens = e_cxt.zip_trait_tokens(cxt, true);
            Some(syn::parse_quote!(#tokens))
        } else {
            None
        };

        let mut deps = Set::new();
        for e_idx in e_cxt.fp_e_deps().iter().cloned() {
            let is_new = deps.insert(cxt[e_idx].e_id().clone());
            if !is_new {
                panic!(
                    "[internal] found two expression types with ID `{}`",
                    cxt[e_idx].e_id(),
                )
            }
        }

        Self {
            e_id,
            res_type_id,
            map,
            ref_trait,
            own_trait,
            deps,
        }
    }

    /// Retrieves a map for a variant.
    pub fn get(&self, v_id: &syn::Ident) -> Res<&Variant> {
        self.map.get(v_id).ok_or_else(|| {
            error!(on v_id =>
                "unknown variant identifier `{}` for `{}` expression type",
                v_id,
                self.e_id,
            )
        })
    }

    /// Zip-trait accessor.
    pub fn get_trait(&self, err_span: Span, is_own: IsOwn) -> Res<&syn::ItemTrait> {
        if is_own {
            self.own_trait.as_ref()
        } else {
            self.ref_trait.as_ref()
        }
        .ok_or_else(|| {
            error!(@err_span =>
                "cannot define zipper on {0} expression, \
                no zip-trait for {0} expressions exists",
                if is_own { "owned" } else { "referenced" }
            )
        })
    }

    /// Signature of a trait function.
    pub fn get_trait_fn(&self, fn_id: &syn::Ident, is_own: IsOwn) -> Res<&syn::TraitItemMethod> {
        for item in &self.get_trait(fn_id.span(), is_own)?.items {
            if let syn::TraitItem::Method(method) = item {
                if &method.sig.ident == fn_id {
                    return Ok(&method);
                }
            }
        }
        bail!(on fn_id => "unknown zip-trait function `{}`", fn_id)
    }
}

implement! {
    impl Expr {
        ToTokens, Display {
            |&self, tokens| {
                let Self { e_id, res_type_id, map, ref_trait, own_trait, deps } = self;
                let variants = map.values();
                let ref_trait = ref_trait.as_ref().map(|ref_trait| {
                    let key = kw::ref_trait(Span::call_site());
                    quote!(#key #ref_trait)
                });
                let own_trait = own_trait.as_ref().map(|own_trait| {
                    let key = kw::own_trait(Span::call_site());
                    quote!(#key #own_trait)
                });
                tokens.extend(quote! {
                    #e_id(#res_type_id #(, #variants)*) {
                        #(#deps),*
                    } {
                        #ref_trait
                        #own_trait
                    }
                })
            }
        }

        Parse {
            |input| {
                let e_id = input.parse()?;

                let paren_content;
                let _paren = syn::parenthesized!(paren_content in input);

                let res_type_id = paren_content.parse()?;
                let _: rust::Token![,] = paren_content.parse()?;
                let variants = Csv::<Variant>::parse_terminated(&paren_content)?;

                let deps = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);

                    let dep_list = Csv::parse_terminated(&brace_content)?;
                    let mut deps = Set::new();
                    for dep in dep_list {
                        let is_new = deps.insert(dep);
                        if !is_new {
                            bail!(@_brace.span =>
                                "[internal] expression types can only appear once"
                            )
                        }
                    }
                    deps
                };

                let (ref_trait, own_trait) = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);

                    let ref_trait = if brace_content.peek(kw::ref_trait) {
                        let _ = brace_content.parse::<kw::ref_trait>()?;
                        Some(brace_content.parse()?)
                    } else {
                        None
                    };
                    let own_trait = if brace_content.peek(kw::own_trait) {
                        let _ = brace_content.parse::<kw::own_trait>()?;
                        Some(brace_content.parse()?)
                    } else {
                        None
                    };

                    if !brace_content.is_empty() {
                        if ref_trait.is_some() && own_trait.is_some() {
                            bail!(
                                brace_content.error("[internal] expected end of expression map")
                            )
                        } else {
                            bail!(
                                brace_content.error(
                                    "[internal] expected ref-zip-trait and own-zip-trait"
                                )
                            )
                        }
                    }

                    (ref_trait, own_trait)
                };

                let mut map = Map::new();
                for variant in variants {
                    let id_span = variant.v_id.span();
                    let prev = map.insert(variant.v_id.clone(), variant);
                    if let Some(variant) = prev {
                        bail!(@id_span =>
                            "[internal] variant `{}` appears twice in expression map",
                            variant.v_id,
                        )
                    }
                }

                Ok(Self {
                    e_id,
                    res_type_id,
                    map,
                    ref_trait,
                    own_trait,
                    deps,
                })
            }
        }
    }
}

/// Variant info.
#[derive(Debug, Clone)]
pub struct Variant {
    /// Variant identifier.
    pub v_id: syn::Ident,
    /// Variant description (struct/tuple/unit).
    pub desc: VariantDesc,
    /// Identifier of the `go_up` zip-trait function.
    pub go_up_id: syn::Ident,
    /// Collection descriptors.
    pub colls: Vec<CollDesc>,
}
impl Variant {
    /// Constructor from a zip context.
    pub fn new(variant: &expr::Variant, cxt: &cxt::ZipCxt) -> Self {
        let (e_idx, v_idx) = (variant.e_idx(), variant.v_idx());
        let e_cxt = &cxt[e_idx];
        let v_id = e_cxt.expr()[v_idx].v_id().clone();
        let desc = VariantDesc::new(variant);
        let go_up_id = variant.zipper_go_up_id().clone();
        let colls = variant
            .data_colls()
            .map(|(d_idx, c_idx)| CollDesc::new(cxt, variant, c_idx, d_idx))
            .collect();
        Self {
            v_id,
            desc,
            go_up_id,
            colls,
        }
    }

    /// Retrieve a collection description from an ident or an index.
    pub fn get_coll_desc(&self, member: &syn::Member) -> Res<&CollDesc> {
        self.colls
            .iter()
            .filter(|coll| &coll.data == member)
            .next()
            .ok_or_else(|| {
                error!(on member =>
                    "unknown collection member `{}`",
                    member.to_token_stream(),
                )
            })
    }
}

implement! {
    impl Variant {
        ToTokens, Display {
            |&self, tokens| {
                let Self {v_id, desc, go_up_id, colls} = self;
                tokens.extend(quote! {
                    #v_id(#desc, #go_up_id #(, #colls)*)
                })
            }
        }

        Parse {
            |input| {
                let v_id = input.parse()?;

                let paren_content;
                let _paren = syn::parenthesized!(paren_content in input);
                let desc = paren_content.parse()?;
                let _: rust::Token![,] = paren_content.parse()?;
                let go_up_id = paren_content.parse()?;

                let colls = if !paren_content.is_empty() {
                    let _: rust::Token![,] = paren_content.parse()?;
                    Csv::parse_terminated(&paren_content)?.into_iter().collect()
                } else {
                    vec![]
                };

                Ok(Self {
                    v_id,
                    desc,
                    go_up_id,
                    colls,
                })
            }
        }
    }
}

/// Describes whether a variant is struct-like, tuple-like or unit-like.
#[derive(Debug, Clone)]
pub enum VariantDesc {
    /// Struct-like variant with the list of fields.
    Struct(Vec<syn::Ident>),
    /// Tuple-like variant with a length.
    Tuple(syn::Index),
    /// Unit-like variant.
    Unit,
}
impl VariantDesc {
    /// Constructor from a zip context.
    pub fn new(variant: &expr::Variant) -> Self {
        match variant.is_struct_like() {
            Some(true) => Self::Struct(
                variant
                    .data()
                    .iter()
                    .map(|data| data.param_id().clone())
                    .collect(),
            ),
            Some(false) => Self::Tuple(syn::Index {
                index: variant.data().len() as u32,
                span: variant.v_id().span(),
            }),
            None => Self::Unit,
        }
    }
}

implement! {
    impl VariantDesc {
        ToTokens, Display {
            |&self, tokens| {
                match self {
                    Self::Struct(fields) => tokens.extend(quote! {
                        { #(#fields),* }
                    }),
                    Self::Tuple(len) => len.to_tokens(tokens),
                    Self::Unit => tokens.extend(quote!(_)),
                }
            }
        }

        Parse {
            |input| {
                let lookahead = input.lookahead1();

                if lookahead.peek(syn::Token![_]) {
                    let _: syn::Token![_] = input.parse()?;
                    Ok(Self::Unit)
                } else if lookahead.peek(syn::LitInt) {
                    Ok(Self::Tuple(input.parse()?))
                } else if lookahead.peek(syn::token::Brace) {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);
                    let fields = Csv::parse_terminated(&brace_content)?;
                    Ok(Self::Struct(fields.into_iter().collect()))
                } else {
                    Err(lookahead.error())
                }
            }
        }
    }
}

/// Description of a collection for some data.
#[derive(Debug, Clone)]
pub struct CollDesc {
    /// Data field/index the collection is for.
    pub data: syn::Member,
    /// Name of the accumulator type in the zip-trait.
    pub acc_type: syn::Ident,
    /// Name of the init function in the zip-trait.
    pub init_fn: syn::Ident,
    /// Name of the fold function in the zip-trait.
    pub fold_fn: syn::Ident,
}
impl CollDesc {
    /// Constructor from a zip context.
    pub fn new(
        cxt: &cxt::ZipCxt,
        variant: &expr::Variant,
        c_idx: idx::Coll,
        d_idx: idx::Data,
    ) -> Self {
        let data = match variant.is_struct_like() {
            Some(true) => syn::Member::Named(variant[d_idx].param_id().clone()),
            Some(false) => syn::Member::Unnamed(syn::Index {
                index: d_idx.get() as u32,
                span: variant[d_idx].param_id().span(),
            }),
            None => panic!("[internal] cannot construct collection description from unit variant"),
        };
        let e_cxt = &cxt[variant.e_idx()];
        let coll_handler = &e_cxt.coll_handlers()[c_idx];
        let acc_type = coll_handler.assoc_acc_typ().clone();
        let init_fn = coll_handler.initializer().id().clone();
        let fold_fn = coll_handler.folder().id().clone();
        Self {
            data,
            acc_type,
            init_fn,
            fold_fn,
        }
    }
}

implement! {
    impl CollDesc {
        ToTokens, Display {
            |&self, tokens| {
                let Self {data, acc_type, init_fn, fold_fn} = self;
                tokens.extend(quote! {
                    #data => { #acc_type, #init_fn, #fold_fn }
                })
            }
        }

        Parse {
            |input| {
                let data = input.parse()?;
                let _: syn::Token![=>] = input.parse()?;

                let brace_content;
                let _brace = syn::braced!(brace_content in input);

                let acc_type = brace_content.parse()?;
                let _: syn::Token![,] = brace_content.parse()?;

                let init_fn = brace_content.parse()?;
                let _: syn::Token![,] = brace_content.parse()?;

                let fold_fn = brace_content.parse()?;

                if !brace_content.is_empty() {
                    let _: syn::Token![,] = brace_content.parse().map_err(
                        |_| input.error("expected trailing comma or closing brace")
                    )?;
                }

                Ok(Self {
                    data,
                    acc_type,
                    init_fn,
                    fold_fn
                })
            }
        }
    }
}
