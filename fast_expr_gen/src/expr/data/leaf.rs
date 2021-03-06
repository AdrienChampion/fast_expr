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

    pub fn frame_typ(&self, _cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        let typ = self.typ.clone();
        if is_own {
            typ
        } else {
            rust::typ::to_expr_ref(typ)
        }
    }
    pub fn frame_der(&self, _cxt: &impl cxt::PreCxtLike, _is_own: IsOwn) -> Option<rust::Typ> {
        None
    }
    pub fn frame_res(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        self.frame_typ(cxt, is_own)
    }
    pub fn zip_res(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        self.frame_res(cxt, is_own)
    }
}
