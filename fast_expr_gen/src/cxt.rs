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
        logln!("{}spec {} {:?}", _pref, self.id(), self.generics())
    }
}

#[derive(Debug, Clone)]
pub struct Cxt {
    /// Specification traits.
    specs: Map<rust::Id, Spec>,

    /// Maps sub-expression identifiers to their indices.
    sub_id_map: Map<rust::Id, idx::Expr>,
    /// Maps sub-expression indices to their context.
    ///
    /// The normal way to access this out of this module is by indexing the context:
    /// `cxt[expr_idx]`.
    subs: idx::ExprMap<ECxt>,
}

impl Cxt {
    fn _new() -> Self {
        Self {
            specs: Map::new(),
            sub_id_map: Map::new(),
            subs: idx::ExprMap::new(),
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
        let e_idx = self.subs.next_index();

        let prev = self.sub_id_map.insert(expr.ident.clone(), e_idx);
        if let Some(prev_idx) = prev {
            let first_ident = self[prev_idx].id();
            bail!(
                on(first_ident, "expression enum `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here")
            )
        }

        let _e_idx = self.subs.push(ECxt::new(self, e_idx, expr)?);
        debug_assert_eq!(e_idx, _e_idx);

        Ok(e_idx)
    }

    /// Constructor from the frontend structure.
    pub fn new(front::Top { specs, exprs }: front::Top) -> Res<Self> {
        let mut slf = Self::_new();

        for spec in specs {
            slf.push_spec(spec)?
        }
        for expr in &exprs {
            let _ = slf.push_expr(expr.clone())?;
        }

        for expr in &slf.subs {
            let e_idx = expr.e_idx();
            let expr = expr::Expr::from_front(&slf, e_idx, &expr.def)?;
            expr.log("");
        }

        Ok(slf)
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        log! {{
            for spec in self.specs.values() {
                spec.log(_pref)
            }
            for ecxt in &self.subs {
                ecxt.log(_pref)
            }
        }}
    }
}

impl Cxt {
    /// Retrieves the context of a sub-expression, if any.
    pub fn get_expr(&self, id: &rust::Id) -> Option<&ECxt> {
        self.sub_id_map.get(id).map(|idx| &self[*idx])
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
    /// Generics associated with this sub-expression.
    generics: Option<rust::Generics>,
    /// Type parameters introduced by `self.generics()`.
    top_t_params: rust::GenericArgs,

    /// Expression definition from the frontend.
    def: rust::Enum,
}

impl ECxt {
    /// Constructor.
    pub fn new(cxt: &Cxt, e_idx: idx::Expr, def: rust::Enum) -> Res<Self> {
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

        Ok(Self {
            e_idx,
            generics,
            top_t_params,
            def,
        })
    }

    /// Index accessor.
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }
    /// Identifier accessor.
    pub fn id(&self) -> &rust::Id {
        &self.def.ident
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
                    "{}{}{:?},",
                    acc,
                    if acc.is_empty() { "" } else { " " },
                    t
                )),
        )
    }
}

implement! {
    impl Cxt {
        Index<idx::Expr, ECxt> {
            |self, idx| &self.subs[idx]
        }
    }
}
