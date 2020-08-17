//! Sub-expression variants.

prelude! {}

pub struct Variant {
    e_idx: idx::Expr,
    v_idx: idx::Variant,

    data: idx::DataMap<expr::Data>,

    src: rust::Variant,
}

impl Variant {
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }
    pub fn v_idx(&self) -> idx::Variant {
        self.v_idx
    }
    pub fn id(&self) -> &rust::Id {
        &self.src.ident
    }
}

impl Variant {
    pub fn from_front(
        cxt: &cxt::Cxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        variant: &rust::Variant,
    ) -> Res<Self> {
        check::expr_variant(variant)?;

        let src = variant.clone();
        let mut data = idx::DataMap::with_capacity(src.fields.len());

        let fields = match &variant.fields {
            syn::Fields::Unit => None,
            syn::Fields::Named(fields) => Some(&fields.named),
            syn::Fields::Unnamed(fields) => Some(&fields.unnamed),
        };
        if let Some(fields) = fields {
            for field in fields {
                let d_idx = data.next_index();
                let field_data = expr::data::Data::from_front(cxt, e_idx, v_idx, d_idx, field)?;
                let _d_idx = data.push(field_data);
                debug_assert_eq!(d_idx, _d_idx)
            }
        }

        Ok(Self {
            e_idx,
            v_idx,

            data,

            src,
        })
    }

    pub fn is_struct_like(&self) -> Option<bool> {
        self.data.iter().next().map(|data| data.id().is_some())
    }

    pub fn log(&self, pref: &str, trailing_comma: bool) {
        let (open, close) = match self.is_struct_like() {
            None => (None, None),
            Some(true) => (Some(" {"), Some("}")),
            Some(false) => (Some(" ("), Some(")")),
        };
        logln!("{}{}{}", pref, self.id(), open.unwrap_or(""));
        for data in &self.data {
            logln!("{}    {},", pref, data)
        }
        if let Some(close) = close {
            logln!("{}{}{}", pref, close, if trailing_comma { "," } else { "" })
        }
    }
}

implement! {
    impl Variant {
        Index<idx::Data, expr::Data> {
            |self, d_idx| &self.data[d_idx]
        }
    }
}
