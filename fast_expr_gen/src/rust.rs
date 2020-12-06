//! Rust structures, mostly exports from [`syn`].
//!
//! [`syn`]: https://docs.rs/syn (syn on crates.io)

prelude! {}

pub mod typ;

pub use proc_macro2::Span;
pub use syn::{
    token, Attribute, Expr, Field, GenericArgument as GenericArg, GenericParam, Generics, Ident,
    ItemEnum as Enum, ItemFn as Fn, ItemTrait as Trait, Lifetime, Lit, Pat, Path, Type,
    TypeParam as TypParam, Variant, Visibility,
};

/// A list of generic arguments.
pub type GenericArgs = Vec<GenericArg>;

/// A list of attributes.
pub type Attributes = Vec<Attribute>;

/// A list of type parameters.
pub type TypParamDefs = Vec<TypParamDef>;

/// A type parameter.
#[derive(Debug, Clone)]
pub enum TypParamDef {
    /// A lifetime identifier.
    Lifetime(Lifetime),
    /// A type identifier.
    Typ(Ident),
}

pub fn try_snake_from(id: &Ident) -> Ident {
    if let Ok(id) = CamelIdent::try_from(id.clone()).and_then(|id| id.to_snake()) {
        id.id
    } else {
        id.clone()
    }
}
pub fn try_camel_from(id: &Ident) -> Ident {
    if let Ok(id) = SnakeIdent::try_from(id.clone()).and_then(|id| id.to_camel()) {
        id.id
    } else {
        id.clone()
    }
}

/// Snake-case identifiers.
#[derive(Debug, Clone)]
pub struct SnakeIdent {
    id: Ident,
}
impl SnakeIdent {
    /// Constructor.
    pub fn new(id: Ident) -> Res<Self> {
        let res = Self { id };
        res.check()?;
        Ok(res)
    }

    /// Turns the identifier in a camel-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_camel(&self) -> Res<CamelIdent> {
        let id = self.id.to_string();
        let mut res = String::with_capacity(id.len());
        let mut make_upp = true;
        for char in id.chars() {
            if char == '_' {
                make_upp = true
            } else if make_upp {
                make_upp = false;
                for char in char.to_uppercase() {
                    res.push(char)
                }
            } else {
                res.push(char)
            }
        }
        Ident::new(&res, self.id.span()).try_into()
    }
}

/// Camel-case identifier.
#[derive(Debug, Clone)]
pub struct CamelIdent {
    id: Ident,
}
impl CamelIdent {
    /// Constructor.
    pub fn new(id: Ident) -> Res<Self> {
        let res = Self { id };
        res.check()?;
        Ok(res)
    }

    /// Turns the identifier in a snake-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_snake(&self) -> Res<SnakeIdent> {
        let id = self.id.to_string();
        let mut res = String::with_capacity(id.len());
        let mut is_first = true;
        for char in id.chars() {
            if char.is_uppercase() {
                if !is_first {
                    res.push('_')
                }
                for char in char.to_lowercase() {
                    res.push(char)
                }
            } else {
                res.push(char);
            }
            is_first = false
        }
        Ident::new(&res, self.id.span()).try_into()
    }
}

implement! {
    impl TypParamDef {
        From<Lifetime> {
            |lt| Self::Lifetime(lt)
        }
        From<Ident> {
            |id| Self::Typ(id)
        }
    }

    impl SnakeIdent {
        TryFrom<Ident, err::Error> {
            |id| Self::new(id)
        }
        TryFrom<CamelIdent, err::Error> {
            |camel| camel.to_snake()
        }
        Deref<Ident> {
            |self| &self.id
        }
        Display {
            |self, fmt| self.id.fmt(fmt)
        }
    }

    impl CamelIdent {
        TryFrom<Ident, err::Error> {
            |id| Self::new(id)
        }
        TryFrom<SnakeIdent, err::Error> {
            |snake| snake.to_camel()
        }
        Deref<Ident> {
            |self| &self.id
        }
        Display {
            |self, fmt| self.id.fmt(fmt)
        }
    }
}

impl SnakeIdent {
    /// Checks that the identifier is indeed a snake-case identifier.
    pub fn check(&self) -> Res<()> {
        let id = self.id.to_string();
        macro_rules! fail {
            () => {
                bail!(on(&self.id, "illegal snake-case identifier {:?}", id))
            };
        }

        let (mut can_upp, mut can_num) = (false, false);

        for char in id.chars() {
            match char {
                '_' => {
                    can_upp = false;
                }
                _ if char.is_ascii_uppercase() => {
                    if !can_upp {
                        fail!()
                    }
                }
                _ if char.is_ascii_digit() => {
                    if !can_num {
                        fail!()
                    }
                }
                _ => {
                    can_upp = true;
                    can_num = true;
                }
            }
        }

        Ok(())
    }
}

impl CamelIdent {
    /// Checks that the identifier is indeed a camel-case identifier.
    pub fn check(&self) -> Res<()> {
        let id = self.id.to_string();
        macro_rules! fail {
            () => {
                bail!(on(&self.id, "illegal camel-case identifier {:?}", id))
            };
        }

        let (mut can_low, mut can_num) = (false, false);

        for char in id.chars() {
            match char {
                '_' => fail!(),
                _ if char.is_ascii_lowercase() => {
                    if !can_low {
                        fail!()
                    }
                }
                _ if char.is_ascii_digit() => {
                    if !can_num {
                        fail!()
                    }
                }
                _ => {
                    can_low = true;
                    can_num = true;
                }
            }
        }

        Ok(())
    }
}
