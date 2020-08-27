//! Sub-expression variants.

prelude! {}

// use expr::frame::Frame;

#[derive(Debug, Clone)]
pub struct Variant {
    e_idx: idx::Expr,
    v_idx: idx::Variant,

    data: idx::DataMap<expr::Data>,
    src: rust::Variant,

    zip_handler_id: rust::Id,
    zipper_go_up_id: rust::Id,
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

    pub fn is_self_rec(&self) -> bool {
        self.data.iter().any(|data| data.is_self_rec())
    }

    pub fn contains_leaf_data(&self) -> bool {
        self.data.iter().any(expr::data::Data::is_leaf)
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

        let zip_handler_id = gen::fun::variant_handler(cxt[e_idx].id(), &src.ident);
        let zipper_go_up_id = gen::fun::go_up(cxt[e_idx].id(), &variant.ident);

        Ok(Self {
            e_idx,
            v_idx,

            data,

            src,

            zip_handler_id,
            zipper_go_up_id,
        })
    }

    pub fn is_struct_like(&self) -> Option<bool> {
        match &self.src.fields {
            syn::Fields::Named(_) => Some(true),
            syn::Fields::Unnamed(_) => Some(false),
            syn::Fields::Unit => None,
        }
    }

    // pub fn frame_variants(&self) -> &idx::DataMap<Option<expr::Frame>> {
    //     &self.frames
    // }
    // pub fn has_frame_variants(&self) -> bool {
    //     self.frames.iter().any(Option::is_some)
    // }

    pub fn zip_handler_id(&self) -> &rust::Id {
        &self.zip_handler_id
    }
    pub fn zipper_go_up_id(&self) -> &rust::Id {
        &self.zipper_go_up_id
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

    pub fn to_constructor_tokens(&self) -> TokenStream {
        let id = self.id();
        let data = self.data.iter().map(|data| data.param_id());
        if self.is_struct_like().unwrap_or(false) {
            quote! {
                #id { #(#data ,)* }
            }
        } else {
            quote! {
                #id ( #(#data ,)* )
            }
        }
    }
}

/// # Expr zipper struct codgen functions
impl Variant {
    pub fn zip_produce_final_res(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        let zip_field = &cxt.zip_ids().self_step_field();
        let go_up = &self.zipper_go_up_id;
        let data_params = self.data.iter().map(expr::data::Data::param_id);
        gen::lib::zip_do::new_go_up(quote! {
            #zip_field . #go_up (
                #( #data_params , )*
            )
        })
    }

    /// Builds the next frame for some data index.
    pub fn zip_handle_variant_from(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: bool,
        d_idx: idx::Data,
    ) -> TokenStream {
        self.data[d_idx].zip_handle_variant_data(cxt, is_own, gen::lib::zip_do::new_go_down, || {
            let mut d_idx = d_idx;
            d_idx.inc();
            if d_idx < self.data.len() {
                self.zip_handle_variant_from(cxt, is_own, d_idx)
            } else {
                self.zip_produce_final_res(cxt)
            }
        })
    }

    pub fn to_zip_handler_fn_tokens(&self, cxt: &cxt::ZipCxt, is_own: bool) -> TokenStream {
        let e_cxt = &cxt[self.e_idx];

        let fun_id = &self.zip_handler_id;
        let out_typ = e_cxt.zip_variant_handler_out_typ(is_own);

        let data_params = self.data.iter().map(|data| {
            let param_id = data.param_id();
            let typ = data.frame_typ(e_cxt, is_own);
            quote! {
                #param_id: #typ
            }
        });

        let handle_variant = self.zip_handle_variant_from(cxt, is_own, idx::Data::zero());

        quote! {
            pub fn #fun_id (
                &mut self,
                #( #data_params , )*
            ) -> #out_typ {
                #handle_variant
            }
        }
    }
}
