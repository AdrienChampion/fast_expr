//! Parsing context.

prelude! {}

#[derive(Debug, Clone)]
pub struct Spec {
    spec: rust::Trait,
}

impl Spec {
    pub fn new(spec: rust::Trait) -> Res<Self> {
        check::spec_trait(&spec)?;
        Ok(Self { spec })
    }

    pub fn id(&self) -> &rust::Id {
        &self.spec.ident
    }
    pub fn generics(&self) -> &rust::Generics {
        &self.spec.generics
    }

    #[inline]
    pub fn log(&self, _pref: impl Display + Copy) {
        logln!(
            "{}spec {} {}",
            _pref,
            self.id(),
            self.generics().to_token_stream()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Cxt {
    /// Specification traits.
    specs: Map<rust::Id, Spec>,

    /// Maps expression identifiers to their indices.
    expr_id_map: Map<rust::Id, idx::Expr>,
    /// Maps expression indices to their context.
    ///
    /// The normal way to access this out of this module is by indexing the context:
    /// `cxt[expr_idx]`.
    exprs: idx::ExprMap<ECxt>,

    /// Maps expression indices to their name.
    ///
    /// This is useful when mutating an expression context so that it can still access the name of
    /// the expressions.
    expr_names: idx::ExprMap<rust::Id>,
}

impl Cxt {
    fn _new() -> Self {
        Self {
            specs: Map::new(),
            expr_id_map: Map::new(),
            exprs: idx::ExprMap::new(),
            expr_names: idx::ExprMap::new(),
        }
    }
    fn push_spec(&mut self, spec: rust::Trait) -> Res<()> {
        let new_span = spec.ident.span();
        let spec = Spec::new(spec.clone())?;

        let prev = self.specs.insert(spec.id().clone(), spec);
        if let Some(prev) = prev {
            let first_ident = prev.id();
            bail!(
                on(first_ident, "specification trait `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here"),
            )
        }

        Ok(())
    }
    fn push_expr(&mut self, expr: rust::Enum) -> Res<idx::Expr> {
        let new_span = expr.ident.span();
        let e_idx = self.exprs.next_index();

        let prev = self.expr_id_map.insert(expr.ident.clone(), e_idx);
        if let Some(prev_idx) = prev {
            let first_ident = self[prev_idx].id();
            bail!(
                on(first_ident, "expression enum `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here")
            )
        }

        let _e_idx = self.expr_names.push(expr.ident.clone());
        debug_assert_eq!(e_idx, _e_idx);
        let _e_idx = self.exprs.push(ECxt::new(self, e_idx, expr)?);
        debug_assert_eq!(e_idx, _e_idx);

        Ok(e_idx)
    }

    /// Constructor from the frontend structure.
    ///
    /// **Warning**: the result of this function is not a fully functional context. It needs to be
    /// [`finalize`]d.
    ///
    /// [`finalize`]: #method.finalize
    fn new(front::Top { specs, exprs }: front::Top) -> Res<Self> {
        let mut slf = Self::_new();

        for spec in specs {
            slf.push_spec(spec)?
        }
        for expr in &exprs {
            let _ = slf.push_expr(expr.clone())?;
        }

        Ok(slf)
    }

    fn finalize(&mut self, exprs: &idx::ExprMap<expr::Expr>) -> Res<()> {
        for (e_idx, expr) in exprs.index_iter() {
            self.exprs[e_idx].finalize(expr, &self.expr_names)?
        }
        Ok(())
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        log! {{
            for spec in self.specs.values() {
                spec.log(_pref)
            }
            for ecxt in &self.exprs {
                ecxt.log(_pref)
            }
        }}
    }
}

impl Cxt {
    /// Retrieves the context of a expression, if any.
    pub fn get_expr(&self, id: &rust::Id) -> Option<&ECxt> {
        self.expr_id_map.get(id).map(|idx| &self[*idx])
    }

    /// Retrieves the specification of an identifier, if any.
    pub fn get_spec(&self, id: &rust::Id) -> Option<&Spec> {
        self.specs.get(id)
    }
}

#[derive(Debug, Clone)]
pub struct ECxt {
    /// Expression index.
    e_idx: idx::Expr,

    /// Expression types mentioned by this expression's definition (includes self).
    ///
    /// **Only valid after finalization.**
    e_deps: Set<idx::Expr>,

    /// Generics associated with this expression.
    generics: Option<rust::Generics>,

    /// Generics of the frame enum for this expression.
    ///
    /// **Only valid after finalization.**
    own_frame_generics: rust::Generics,
    /// Same as `own_frame_generics`, but with the expression lifetime added as the first parameter.
    ///
    /// **Only valid after finalization.**
    ref_frame_generics: rust::Generics,

    /// Type parameters introduced by `self.generics()`.
    top_t_params: rust::GenericArgs,

    /// Frame-type identifier.
    frame_typ_id: rust::Id,

    /// Expression definition from the frontend.
    def: rust::Enum,
}

impl ECxt {
    /// Constructor.
    ///
    /// **Warning**: the result of this function is not a fully functional context. It needs to be
    /// [`finalize`]d.
    ///
    /// [`finalize`]: #method.finalize
    fn new(cxt: &Cxt, e_idx: idx::Expr, def: rust::Enum) -> Res<Self> {
        let generics = {
            let mut params = def.generics.params.iter();
            match (params.next(), params.next()) {
                (Some(rust::GenericParam::Type(maybe_spec)), None) => {
                    if let Some(spec) = cxt.get_spec(&maybe_spec.ident) {
                        Some(spec.generics().clone())
                    } else {
                        None
                    }
                }
                _ => None,
            }
        };

        let own_frame_generics = generics.as_ref().unwrap_or(&def.generics).clone();
        let ref_frame_generics = {
            use syn::*;

            let mut generics = own_frame_generics.clone();
            let params = std::mem::replace(&mut generics.params, punctuated::Punctuated::new());

            let expr_lt = GenericParam::Lifetime(LifetimeDef {
                attrs: vec![],
                lifetime: gen::lifetime::expr(),
                colon_token: None,
                bounds: punctuated::Punctuated::new(),
            });

            generics.params.push(expr_lt);
            generics.params.extend(params);

            generics
        };

        let top_t_params = {
            let generics = generics.as_ref().unwrap_or(&def.generics);
            generics
                .params
                .iter()
                .map(|param| {
                    use rust::{GenericArg, GenericParam::*};
                    match param {
                        Type(typ) => {
                            let typ = &typ.ident;
                            GenericArg::Type(syn::parse_quote!(#typ))
                        }
                        Lifetime(lt) => GenericArg::Lifetime(lt.lifetime.clone()),
                        Const(cst) => {
                            let cst = &cst.ident;
                            GenericArg::Const(syn::parse_quote!(#cst))
                        }
                    }
                })
                .collect()
        };

        let e_deps = Set::new();

        let frame_typ_id = gen::frame::typ_id(&def.ident);

        Ok(Self {
            e_idx,
            e_deps,

            generics,
            ref_frame_generics,
            own_frame_generics,

            top_t_params,

            frame_typ_id,

            def,
        })
    }

    fn finalize(&mut self, expr: &expr::Expr, expr_info: &idx::ExprMap<rust::Id>) -> Res<()> {
        // Register all expression types mentioned by this expression's data.
        for variant in expr.variants() {
            for data in variant.data() {
                data.map_rec_exprs(|e_idx| {
                    let _is_new = self.add_dep(e_idx);
                    Ok(())
                })?
            }
        }

        // Add revelant type parameters for the results of these expressions to the frame generics.
        for e_idx in self.e_deps.iter().cloned() {
            let ident = gen::typ::res(&expr_info[e_idx]);

            let typ_param = syn::TypeParam {
                attrs: vec![],
                ident,
                colon_token: None,
                bounds: syn::punctuated::Punctuated::new(),
                eq_token: None,
                default: None,
            };

            self.own_frame_generics
                .params
                .push(syn::GenericParam::Type(typ_param.clone()));
            self.ref_frame_generics
                .params
                .push(syn::GenericParam::Type(typ_param.clone()));
        }

        Ok(())
    }

    /// Index accessor.
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }

    /// Identifier accessor.
    pub fn id(&self) -> &rust::Id {
        &self.def.ident
    }
    /// Frame-type identifier.
    pub fn frame_typ_id(&self) -> &rust::Id {
        &self.frame_typ_id
    }

    /// Generics accessor.
    pub fn generics(&self) -> &rust::Generics {
        self.generics.as_ref().unwrap_or(&self.def.generics)
    }
    /// Types introduced in the generics, as arguments.
    pub fn top_t_params(&self) -> &rust::GenericArgs {
        &self.top_t_params
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        logln!(
            "{}expr({}) {}<{}>",
            _pref,
            self.e_idx(),
            self.id(),
            self.top_t_params
                .iter()
                .fold(String::new(), |acc, t| format!(
                    "{}{}{},",
                    acc,
                    if acc.is_empty() { "" } else { " " },
                    t.to_token_stream(),
                )),
        )
    }

    fn add_dep(&mut self, e_idx: idx::Expr) -> bool {
        self.e_deps.insert(e_idx)
    }
}

impl ECxt {
    pub fn frame_generics(&self, is_own: IsOwn) -> &rust::Generics {
        if is_own {
            &self.own_frame_generics
        } else {
            &self.ref_frame_generics
        }
    }
}

implement! {
    impl Cxt {
        Index<idx::Expr, ECxt> {
            |self, idx| &self.exprs[idx]
        }
    }
}

#[derive(Debug, Clone)]
pub struct Top {
    pub cxt: Cxt,
    pub exprs: idx::ExprMap<expr::Expr>,
}
impl Top {
    pub fn new(top: front::Top) -> Res<Self> {
        let mut cxt = Cxt::new(top)?;
        let exprs = {
            let mut exprs = idx::ExprMap::with_capacity(cxt.exprs.len());
            for expr in &cxt.exprs {
                let e_idx = expr.e_idx();
                let expr = expr::Expr::from_front(&cxt, e_idx, &expr.def)?;
                let _e_idx = exprs.push(expr);
                debug_assert_eq!(e_idx, _e_idx)
            }
            exprs
        };
        cxt.finalize(&exprs)?;
        Ok(Self { cxt, exprs })
    }

    pub fn log(&self, pref: &str) {
        let sub_pref = &format!("{}    ", pref);
        logln!("cxt {{");
        self.cxt.log(sub_pref);
        logln!("}}");
        logln!("exprs {{");
        for expr in &self.exprs {
            expr.log(sub_pref);
        }
        logln!("}}");
    }
}

impl Top {
    pub fn to_expr_enum_tokens(&self, stream: &mut TokenStream) {
        for expr in &self.exprs {
            expr.to_expr_enum_tokens(stream)
        }
    }

    pub fn to_zip_tokens_for(&self, stream: &mut TokenStream, is_own: IsOwn) {
        let (zip_doc, zip_mod) = (gen::doc::module::zip(is_own), gen::module::zip(is_own));

        let zip_tokens = self
            .exprs
            .iter()
            .map(|expr| expr.to_zip_tokens(&self.cxt, is_own));

        stream.extend(quote! {
            #[doc = #zip_doc]
            pub mod #zip_mod {
                use super::*;

                #(#zip_tokens)*
            }
        })
    }

    pub fn to_zip_tokens(&self, stream: &mut TokenStream) {
        self.to_zip_tokens_for(stream, true);
        self.to_zip_tokens_for(stream, false);
    }
}

impl ToTokens for Top {
    fn to_tokens(&self, stream: &mut TokenStream) {
        self.to_expr_enum_tokens(stream);
        self.to_zip_tokens(stream);
    }
}
