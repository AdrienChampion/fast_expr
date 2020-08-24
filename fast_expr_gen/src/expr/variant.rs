//! Sub-expression variants.

prelude! {}

// use expr::frame::Frame;

#[derive(Debug, Clone)]
pub struct Variant {
    e_idx: idx::Expr,
    v_idx: idx::Variant,

    data: idx::DataMap<expr::Data>,
    src: rust::Variant,
}

implement! {
    impl Variant {
        Index<idx::Data, expr::Data> {
            |self, d_idx| &self.data[d_idx]
        }
    }
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
    pub fn data(&self) -> &idx::DataMap<expr::Data> {
        &self.data
    }
}

impl Variant {
    pub fn from_front(
        cxt: &mut cxt::PreCxt,
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

    // pub fn frame_variants(&self) -> &idx::DataMap<Option<expr::Frame>> {
    //     &self.frames
    // }
    // pub fn has_frame_variants(&self) -> bool {
    //     self.frames.iter().any(Option::is_some)
    // }

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

/// # Expr-enum codegen functions.
impl Variant {
    fn to_fields_tokens(
        &self,
        stream: &mut TokenStream,
        fields: &syn::punctuated::Punctuated<rust::Field, syn::token::Comma>,
    ) {
        debug_assert_eq!(fields.len(), self.data.len());
        let mut d_idx = idx::Data::zero();
        gen::punct::do_with(fields, |punct_opt| {
            self.data[d_idx].to_expr_data_tokens(stream);
            d_idx.inc();
            if let Some(punct) = punct_opt {
                punct.to_tokens(stream)
            }
        })
    }

    pub fn to_expr_variant_tokens(&self, stream: &mut TokenStream) {
        stream.append_all(&self.src.attrs);
        self.src.ident.to_tokens(stream);

        logln!("generating {}", self.src.ident);

        use syn::Fields::*;
        match &self.src.fields {
            Named(fields) => fields.brace_token.surround(stream, |stream| {
                self.to_fields_tokens(stream, &fields.named)
            }),
            Unnamed(fields) => fields.paren_token.surround(stream, |stream| {
                self.to_fields_tokens(stream, &fields.unnamed)
            }),
            Unit => (),
        }

        if let Some((eq_token, disc)) = &self.src.discriminant {
            eq_token.to_tokens(stream);
            disc.to_tokens(stream);
        }
    }
}
