//! Parsing context.

prelude! {}

pub fn from_front(front::Expr { top, subs, .. }: front::Expr) -> Result<Cxt> {
    let (id, top_t_params) = (top.ident, top.generics);

    logln!("building context for {}", id);

    let mut cxt = Cxt::new(id, top_t_params)?;

    for sub in subs {
        logln!("registering sub-expression {}", sub.ident);
        let idx = cxt.push_sub(&sub)?;
        logln!("-> Expr({})", idx);
    }

    Ok(cxt)
}

#[derive(Debug, Clone)]
pub struct Cxt {
    /// Name of the top-level expression (original span).
    id: rust::Id,
    /// Type parameters declared at the top-level, and their constraints (original span).
    generics: rust::Generics,
    /// The lifetimes/type identifiers introduced in `self.generics` (same span as the generics).
    t_params: rust::TypParams,

    /// Maps sub-expression identifiers to their indices.
    sub_id_map: Map<rust::Id, idx::Expr>,
    /// Maps sub-expression indices to their context.
    ///
    /// The normal way to access this out of this module is by indexing the context:
    /// `cxt[expr_idx]`.
    subs: idx::ExprMap<SubCxt>,
}

impl Cxt {
    /// Constructor.
    pub fn new(id: rust::Id, generics: rust::Generics) -> Result<Self> {
        // Extract type parameter identifiers.
        let mut t_params: rust::TypParams = Vec::with_capacity(generics.params.len());
        for param in &generics.params {
            use syn::GenericParam::*;
            match param {
                Type(typ_param) => t_params.push(typ_param.ident.clone().into()),
                Lifetime(lt_def) => t_params.push(lt_def.lifetime.clone().into()),
                Const(const_param) => bail!(
                    for(const_param, "const parameters are not supported")
                ),
            }
        }

        Ok(Self {
            id,
            generics,
            t_params,

            sub_id_map: Map::new(),
            subs: idx::ExprMap::new(),
        })
    }

    /// Pushes a sub-expression name.
    fn push_sub(&mut self, sub: &rust::Enum) -> Result<idx::Expr> {
        let idx = self.subs.next_index();

        let prev = self.sub_id_map.insert(sub.ident.clone(), idx);
        if let Some(prev_idx) = prev {
            let id = self.subs[prev_idx].id.to_string();
            bail!(
                for(
                    &self.subs[prev_idx].id,
                    "the name `{}` is defined multiple times", id
                ),
                @(sub.ident.span(), "redefinition of `{}` here", id),
            )
        }

        let sub = SubCxt::new(self, sub);
        let _idx = self.subs.push(sub);
        debug_assert_eq!(idx, _idx);

        Ok(idx)
    }
}

impl Cxt {
    /// Retrieves the context of a sub-expression, if any.
    pub fn sub_cxt_of(&self, id: &rust::Id) -> Option<&SubCxt> {
        self.sub_id_map.get(id).map(|idx| &self[*idx])
    }
}

#[derive(Debug, Clone)]
pub struct SubCxt {
    /// Sub-expression identifier (original span).
    id: rust::Id,
    /// Generics associated with this sub-expression (original span).
    generics: rust::Generics,
}

impl SubCxt {
    pub fn new(cxt: &Cxt, sub: &rust::Enum) -> Self {
        let id = sub.ident.clone();
        let generics = if sub.generics.params.is_empty() {
            cxt.generics.clone()
        } else {
            sub.generics.clone()
        };
        Self { id, generics }
    }

    /// Identifier accessor.
    pub fn id(&self) -> &rust::Id {
        &self.id
    }
    /// Generics accessor.
    pub fn generics(&self) -> &rust::Generics {
        &self.generics
    }
}

implement! {
    impl Cxt {
        Index<idx::Expr, SubCxt> {
            |self, idx| &self.subs[idx]
        }
    }
}
