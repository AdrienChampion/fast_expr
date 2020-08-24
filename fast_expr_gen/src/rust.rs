//! Rust structures, mostly exports from [`syn`].
//!
//! [`syn`]: https://docs.rs/syn (syn on crates.io)

prelude! {}

pub mod typ;

pub use proc_macro2::Span;
pub use syn::{
    Attribute, Field, GenericArgument as GenericArg, GenericParam, Generics, Ident as Id,
    ItemEnum as Enum, ItemTrait as Trait, Lifetime, Path, Type as Typ, TypeParam as TypParam,
    Variant,
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
    Typ(Id),
}

/// Snake-case identifiers.
#[derive(Debug, Clone)]
pub struct SnakeId {
    id: Id,
}
impl SnakeId {
    /// Constructor.
    pub fn new(id: Id) -> Res<Self> {
        let res = Self { id };
        res.check()?;
        Ok(res)
    }

    /// Turns the identifier in a camel-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_camel(&self) -> Res<CamelId> {
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
        Id::new(&res, self.id.span()).try_into()
    }
}

/// Camel-case identifier.
#[derive(Debug, Clone)]
pub struct CamelId {
    id: Id,
}
impl CamelId {
    /// Constructor.
    pub fn new(id: Id) -> Res<Self> {
        let res = Self { id };
        res.check()?;
        Ok(res)
    }

    /// Turns the identifier in a snake-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_snake(&self) -> Res<SnakeId> {
        let id = self.id.to_string();
        let mut res = String::with_capacity(id.len());
        let mut insert_us = false;
        for char in id.chars() {
            if char.is_uppercase() {
                if insert_us {
                    res.push('_')
                }
                for char in char.to_lowercase() {
                    res.push(char)
                }
            } else {
                res.push(char);
                insert_us = true
            }
        }
        Id::new(&res, self.id.span()).try_into()
    }
}

implement! {
    impl TypParamDef {
        From<Lifetime> {
            |lt| Self::Lifetime(lt)
        }
        From<Id> {
            |id| Self::Typ(id)
        }
    }

    impl SnakeId {
        TryFrom<Id, err::Error> {
            |id| Self::new(id)
        }
        TryFrom<CamelId, err::Error> {
            |camel| camel.to_snake()
        }
        Deref<Id> {
            |self| &self.id
        }
        Display {
            |self, fmt| self.id.fmt(fmt)
        }
    }

    impl CamelId {
        TryFrom<Id, err::Error> {
            |id| Self::new(id)
        }
        TryFrom<SnakeId, err::Error> {
            |snake| snake.to_camel()
        }
        Deref<Id> {
            |self| &self.id
        }
        Display {
            |self, fmt| self.id.fmt(fmt)
        }
    }
}

impl SnakeId {
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

impl CamelId {
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
