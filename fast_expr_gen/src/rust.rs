//! Rust structures, mostly exports from [`syn`].
//!
//! [`syn`]: https://docs.rs/syn (syn on crates.io)

pub use syn::{Generics, Ident as Id, ItemEnum as Enum, Lifetime, Type as Typ};

pub type TypParams = Vec<TypParam>;

#[derive(Debug, Clone)]
pub enum TypParam {
    Lifetime(Lifetime),
    Typ(Id),
}

#[derive(Debug, Clone)]
pub struct SnakeId {
    id: Id,
}
impl SnakeId {
    /// Constructor.
    pub fn new(id: Id) -> Self {
        let res = Self { id };
        res.check();
        res
    }

    /// Turns the identifier in a camel-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_camel(&self) -> CamelId {
        let id = self.id.to_string();
        let mut res = String::with_capacity(id.len());
        let mut make_upp = true;
        for char in id.chars() {
            if char == '_' {
                make_upp = true
            } else if make_upp {
                for char in char.to_uppercase() {
                    res.push(char)
                }
            } else {
                res.push(char)
            }
        }
        Id::new(&res, self.id.span()).into()
    }
}

#[derive(Debug, Clone)]
pub struct CamelId {
    id: Id,
}
impl CamelId {
    /// Constructor.
    pub fn new(id: Id) -> Self {
        let res = Self { id };
        res.check();
        res
    }

    /// Turns the identifier in a snake-case identifier.
    ///
    /// The span of the resulting identifier is the same as `self`.
    pub fn to_snake(&self) -> SnakeId {
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
        Id::new(&res, self.id.span()).into()
    }
}

implement! {
    impl TypParam {
        From<Lifetime> {
            |lt| Self::Lifetime(lt)
        }
        From<Id> {
            |id| Self::Typ(id)
        }
    }

    impl SnakeId {
        From<Id> {
            |id| Self::new(id)
        }
        From<CamelId> {
            |camel| camel.to_snake()
        }
        Deref<Id> {
            |self| &self.id
        }
    }

    impl CamelId {
        From<Id> {
            |id| Self::new(id)
        }
        From<SnakeId> {
            |snake| snake.to_camel()
        }
        Deref<Id> {
            |self| &self.id
        }
    }
}

impl SnakeId {
    /// Checks that the identifier is indeed a snake-case identifier.
    #[cfg(not(debug_assertions))]
    pub fn check(&self) {}
    /// Checks that the identifier is indeed a snake-case identifier.
    #[cfg(debug_assertions)]
    pub fn check(&self) {
        let id = self.id.to_string();
        macro_rules! fail {
            () => {
                panic!("illegal snake-case identifier {:?}", id)
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
    }
}

impl CamelId {
    /// Checks that the identifier is indeed a camel-case identifier.
    #[cfg(not(debug_assertions))]
    pub fn check(&self) {}
    /// Checks that the identifier is indeed a camel-case identifier.
    #[cfg(debug_assertions)]
    pub fn check(&self) {
        let id = self.id.to_string();
        macro_rules! fail {
            () => {
                panic!("illegal camel-case identifier {:?}", id)
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
    }
}