//! Sub-expression variants.

prelude! {}

/// A variant, stored inside a [`crate::expr::Expr`].
#[derive(Debug, Clone)]
pub struct Variant {
    /// Expression index.
    e_idx: idx::Expr,
    /// Variant index.
    v_idx: idx::Variant,

    /// Variant's data.
    data: idx::DataMap<expr::Data>,

    /// Identifier of the handler function for this variant.
    zip_handler_id: Ident,
    /// Identifier of the `go_up` function for this variant.
    zipper_go_up_id: Ident,

    /// Variant frontend version.
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
    /// Expression index.
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }
    /// Variant index.
    pub fn v_idx(&self) -> idx::Variant {
        self.v_idx
    }

    /// Variant id.
    pub fn v_id(&self) -> &Ident {
        &self.src.ident
    }

    /// Data accessor.
    pub fn data(&self) -> &idx::DataMap<expr::Data> {
        &self.data
    }

    /// True if the variant mentions the expression type it belongs to.
    pub fn is_self_rec(&self) -> bool {
        self.data.iter().any(|data| data.is_self_rec())
    }

    /// True if the variant contains leaf-like data.
    pub fn contains_leaf_data(&self) -> bool {
        self.data.iter().any(|data| data.is_leaf())
    }

    /// True if the variant is leaf-like, *i.e.* does not mention any expression type.
    pub fn is_leaf(&self) -> bool {
        self.data.iter().all(|data| data.is_leaf())
    }
}

impl Variant {
    /// Constructor from a frontend representation.
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

        let zip_handler_id = gen::fun::variant_handler(cxt[e_idx].e_id(), &src.ident);
        let zipper_go_up_id = gen::fun::go_up(cxt[e_idx].e_id(), &variant.ident);

        Ok(Self {
            e_idx,
            v_idx,

            data,

            src,

            zip_handler_id,
            zipper_go_up_id,
        })
    }

    /// True if the variant is struct-like.
    ///
    /// Returns `None` if the variant is unit-like.
    pub fn is_struct_like(&self) -> Option<bool> {
        match &self.src.fields {
            syn::Fields::Named(_) => Some(true),
            syn::Fields::Unnamed(_) => Some(false),
            syn::Fields::Unit => None,
        }
    }

    /// Identifier of this variant's handler function.
    pub fn zip_handler_id(&self) -> &Ident {
        &self.zip_handler_id
    }
    /// Identifier of thi variant's `go_up` function.
    pub fn zipper_go_up_id(&self) -> &Ident {
        &self.zipper_go_up_id
    }

    /// Logs itself.
    pub fn log(&self, pref: &str, trailing_comma: bool) {
        let (open, close) = match self.is_struct_like() {
            None => (None, None),
            Some(true) => (Some(" {"), Some("}")),
            Some(false) => (Some(" ("), Some(")")),
        };
        logln!("{}{}{}", pref, self.v_id(), open.unwrap_or(""));
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
    /// Generates code for the fields of the variant.
    fn to_fields_tokens(
        &self,
        stream: &mut TokenStream,
        fields: &syn::punctuated::Punctuated<rust::Field, rust::token::Comma>,
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

    /// Generates code for the full variant in an expression type definition.
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

    /// Generates code for constructing this variant.
    ///
    /// The code just aggregates the data identifiers expected by this variant. It is the caller's
    /// responsability to have these identifiers in scope, with whatever value the caller wants.
    ///
    /// Also, the variant's name is **not** prefixed with the expression's identifier.
    pub fn to_constructor_tokens(&self, stream: &mut TokenStream) {
        let id = self.v_id();
        let data = self.data.iter().map(|data| data.param_id());
        stream.extend(match self.is_struct_like() {
            Some(true) => quote! {
                #id { #(#data ,)* }
            },
            Some(false) => quote! {
                #id ( #(#data ,)* )
            },
            None => quote! { #id },
        })
    }
}

/// # Expr zipper struct codegen functions
impl Variant {
    /// Codegen for the call to this variant's `go_up` function.
    pub fn to_go_up_call_tokens(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        let zip_field = &cxt.zip_ids().self_step_field();
        let go_up = &self.zipper_go_up_id;
        let data_params = self.data.iter().map(expr::data::Data::param_id);
        let empty_convert = cxt.lib_gen().zip_do_empty_convert();
        quote! {
            #zip_field . #go_up (
                #( #data_params , )*
            ) . #empty_convert ()
        }
    }

    /// Builds the next frame for some data index.
    pub fn to_next_frame_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: bool,
        d_idx: idx::Data,
    ) -> TokenStream {
        self.data[d_idx].zip_handle_variant_data(
            cxt,
            is_own,
            |input| cxt.lib_gen().zip_do_new_go_down(input),
            || {
                let mut d_idx = d_idx;
                d_idx.inc();
                if d_idx < self.data.len() {
                    self.to_next_frame_tokens(cxt, is_own, d_idx)
                } else {
                    self.to_go_up_call_tokens(cxt)
                }
            },
        )
    }

    /// Codegen for this variant's handler function.
    pub fn to_zip_handler_fn_tokens(&self, cxt: &cxt::ZipCxt, is_own: bool) -> TokenStream {
        let e_cxt = &cxt[self.e_idx];

        let fun_id = &self.zip_handler_id;
        let out_typ = e_cxt.zip_variant_handler_out_typ(cxt, is_own);

        let data_params = self.data.iter().map(|data| {
            let param_id = data.param_id();
            let typ = data.frame_typ(cxt, is_own);
            quote! {
                #param_id: #typ
            }
        });

        let handle_variant = self.to_next_frame_tokens(cxt, is_own, idx::Data::zero());

        let vis = cxt.conf().secret_item_vis();

        quote! {
            #vis fn #fun_id (
                &mut self,
                #( #data_params , )*
            ) -> #out_typ {
                #handle_variant
            }
        }
    }
}
