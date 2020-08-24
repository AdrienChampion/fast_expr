//! Non-recursive data handling.

prelude! {}

/// A leaf is just some data of some type that's not a sub-expression type.
#[derive(Debug, Clone)]
pub struct Leaf {
    typ: rust::Typ,
}
impl Leaf {
    /// Constructor.
    pub fn new(typ: rust::Typ) -> Self {
        Self { typ }
    }

    pub fn map_rec_exprs(&self, _: impl FnMut(idx::Expr) -> Res<()>) -> Res<()> {
        Ok(())
    }
}

impl Leaf {
    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }

    #[inline]
    pub fn needs_frame(&self) -> bool {
        false
    }

    pub fn frame_typ(&self, _e_cxt: &cxt::pre::ECxt, is_own: IsOwn) -> rust::Typ {
        let typ = self.typ.clone();
        if is_own {
            typ
        } else {
            rust::typ::to_expr_ref(typ)
        }
    }
    pub fn frame_der(&self, _e_cxt: &cxt::pre::ECxt, _is_own: IsOwn) -> Option<rust::Typ> {
        None
    }
    pub fn frame_res(&self, e_cxt: &cxt::pre::ECxt, is_own: IsOwn) -> rust::Typ {
        self.frame_typ(e_cxt, is_own)
    }
}
