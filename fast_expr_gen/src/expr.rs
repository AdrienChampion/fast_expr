//! Expression structures.

prelude! {}

pub mod data;
pub mod variant;

use self::{data::Data, variant::Variant};

pub struct Expr {
    e_idx: idx::Expr,

    variants: idx::VariantMap<Variant>,
    variant_map: Map<rust::Id, idx::Variant>,

    src: rust::Enum,
}

impl Expr {
    pub fn from_front(cxt: &cxt::Cxt, e_idx: idx::Expr, src: &rust::Enum) -> Res<Self> {
        let src = src.clone();
        let mut variants = idx::VariantMap::<Variant>::with_capacity(src.variants.len());
        let mut variant_map = Map::new();

        for variant in &src.variants {
            let v_idx = variants.next_index();
            let variant = Variant::from_front(cxt, e_idx, v_idx, variant)?;

            let prev = variant_map.insert(variant.id().clone(), v_idx);
            if let Some(prev_idx) = prev {
                bail!(
                    on(
                        variants[prev_idx].id(),
                        "expression enum `{}` is defined multiple times",
                        variant.id(),
                    ),
                    on(variant.id(), "redefined here")
                )
            }

            let _v_idx = variants.push(variant);
            debug_assert_eq!(v_idx, _v_idx);
        }

        Ok(Self {
            e_idx,

            variants,
            variant_map,

            src,
        })
    }
}

impl Expr {
    // pub fn new(e_idx: idx::Expr, id: rust::Id) -> Self {
    //     Self {
    //         e_idx,
    //         id,

    //         variants: idx::VariantMap::new(),
    //         variant_map: Map::new(),
    //     }
    // }

    pub fn id(&self) -> &rust::Id {
        &self.src.ident
    }
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }

    pub fn variants(&self) -> &idx::VariantMap<Variant> {
        &self.variants
    }

    pub fn push_variant(&mut self, variant: Variant) -> Res<idx::Variant> {
        let v_idx = self.variants.next_index();

        let prev = self.variant_map.insert(variant.id().clone(), v_idx);
        if let Some(prev_idx) = prev {
            bail!(
                on(
                    self[prev_idx].id(),
                    "variant `{}` is defined multiple times",
                    variant.id()
                ),
                on(variant.id(), "re-defined here"),
            )
        }

        let _v_idx = self.variants.push(variant);
        debug_assert_eq!(v_idx, _v_idx);

        Ok(v_idx)
    }

    pub fn log(&self, pref: &str) {
        logln!("{}expr {}<...> {{", pref, self.id());
        let sub_pref = &format!("{}    ", pref);
        for variant in &self.variants {
            variant.log(sub_pref, true)
        }
        logln!("{}}}", pref);
    }
}

implement! {
    impl Expr {
        Index<idx::Variant, Variant> {
            |self, v_idx| &self.variants[v_idx]
        }
    }
}
