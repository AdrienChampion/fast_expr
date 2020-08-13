//! Variant data representation.

prelude! {}

/// Some variant data.
pub struct Data {
    ident: Option<rust::SnakeId>,
}

/// Trait providing type-derivation-related functionalities.
pub trait DataExt {
    /// Type of the data.
    fn typ(&self) -> &rust::Typ;

    /// Derivative of the type of the data.
    fn der(&self) -> Option<&rust::Typ>;
}

pub enum DataTyp {
    Leaf(Leaf),
}
impl DataExt for DataTyp {
    fn typ(&self) -> &rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.typ(),
        }
    }
    fn der(&self) -> Option<&rust::Typ> {
        match self {
            Self::Leaf(leaf) => leaf.der(),
        }
    }
}

/// A leaf is just some data of some type that's not a sub-expression type.
pub struct Leaf {
    typ: rust::Typ,
}
impl Leaf {
    /// Constructor.
    pub fn new(typ: rust::Typ) -> Self {
        Self { typ }
    }
}

impl DataExt for Leaf {
    fn typ(&self) -> &rust::Typ {
        &self.typ
    }

    fn der(&self) -> Option<&rust::Typ> {
        None
    }
}
