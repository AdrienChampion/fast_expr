//! Variant frames.

prelude! {}

#[derive(Debug, Clone)]
pub struct Frame {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,

    own_def: rust::Variant,
    ref_def: rust::Variant,
}

impl Frame {
    pub fn new(
        e_cxt: &cxt::ECxt,
        pivot_d_idx: idx::Data,
        data: &idx::DataMap<expr::Data>,
    ) -> Option<Self> {
        let pivot = &data[pivot_d_idx];

        if !pivot.needs_frame() {
            return None;
        }

        let (e_idx, v_idx) = (pivot.e_idx(), pivot.v_idx());
        let ident = gen::frame::variant_id(e_cxt.v_id(v_idx), pivot_d_idx, pivot.id());

        let (own_fields, ref_fields) = {
            let mut own_fields = syn::punctuated::Punctuated::new();
            let mut ref_fields = syn::punctuated::Punctuated::new();

            for (curr_d_idx, curr_data) in data.index_iter() {
                macro_rules! push_fields {
                    (@ident_colon) => {{
                        curr_data.id().map(|id|
                            (Some(id.clone()), Some(syn::token::Colon::default()))
                        ).unwrap_or((None, None))
                    }};

                    (@own $own_typ:expr) => {{
                        let (ident, colon_token) = push_fields!(@ident_colon);
                        own_fields.push(rust::Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            ident: ident,
                            colon_token: colon_token,
                            ty: $own_typ,
                        })
                    }};
                    (@ref $ref_typ:expr) => {{
                        let (ident, colon_token) = push_fields!(@ident_colon);
                        ref_fields.push(rust::Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            ident: ident,
                            colon_token: colon_token,
                            ty: $ref_typ,
                        })
                    }};

                    (opt $($mk_typ_fn:tt)*) => {{
                        if let Some(own_typ) = $($mk_typ_fn)*(e_cxt, true) {
                            push_fields!(@own own_typ)
                        }
                        if let Some(ref_typ) = $($mk_typ_fn)*(e_cxt, false) {
                            push_fields!(@ref ref_typ)
                        }
                    }};
                    ($($mk_typ_fn:tt)*) => {{
                        push_fields!(@own $($mk_typ_fn)*(e_cxt, true));
                        push_fields!(@ref $($mk_typ_fn)*(e_cxt, false));
                    }};
                }

                if curr_d_idx < pivot_d_idx {
                    push_fields!(curr_data.frame_res)
                } else if curr_d_idx == pivot_d_idx {
                    push_fields!(opt curr_data.frame_der)
                } else {
                    debug_assert!(curr_d_idx > pivot_d_idx);
                    push_fields!(curr_data.frame_typ)
                }
            }

            if pivot.id().is_some() {
                (
                    syn::Fields::Named(syn::FieldsNamed {
                        brace_token: syn::token::Brace::default(),
                        named: own_fields,
                    }),
                    syn::Fields::Named(syn::FieldsNamed {
                        brace_token: syn::token::Brace::default(),
                        named: ref_fields,
                    }),
                )
            } else {
                (
                    syn::Fields::Unnamed(syn::FieldsUnnamed {
                        paren_token: syn::token::Paren::default(),
                        unnamed: own_fields,
                    }),
                    syn::Fields::Unnamed(syn::FieldsUnnamed {
                        paren_token: syn::token::Paren::default(),
                        unnamed: ref_fields,
                    }),
                )
            }
        };

        let own_def = rust::Variant {
            attrs: vec![],
            ident: ident.clone(),
            fields: own_fields,
            discriminant: None,
        };
        let ref_def = rust::Variant {
            attrs: vec![],
            ident,
            fields: ref_fields,
            discriminant: None,
        };

        Some(Self {
            e_idx,
            v_idx,
            d_idx: pivot_d_idx,

            own_def,
            ref_def,
        })
    }

    pub fn def(&self, is_own: IsOwn) -> &rust::Variant {
        if is_own {
            &self.own_def
        } else {
            &self.ref_def
        }
    }
}
