//! Contexts with frame information.

prelude! {}

use cxt::pre;

#[derive(Debug, Clone)]
pub struct ExprDeps {
    fp_e_deps: Set<idx::Expr>,
    own_frame_generics: rust::Generics,
    ref_frame_generics: rust::Generics,
}
impl ExprDeps {
    pub fn new(e_cxt: &cxt::pre::ECxt) -> Self {
        let own_frame_generics = e_cxt.generics().clone();
        let ref_frame_generics = {
            use syn::*;

            let mut generics = own_frame_generics.clone();
            let params = std::mem::replace(&mut generics.params, punctuated::Punctuated::new());

            let expr_lt = GenericParam::Lifetime(LifetimeDef {
                attrs: vec![],
                lifetime: gen::lifetime::expr(),
                colon_token: None,
                bounds: punctuated::Punctuated::new(),
            });

            generics.params.push(expr_lt);
            generics.params.extend(params);

            generics
        };
        Self {
            fp_e_deps: Set::new(),
            own_frame_generics,
            ref_frame_generics,
        }
    }

    pub fn fp_e_deps_mut(&mut self) -> &mut Set<idx::Expr> {
        &mut self.fp_e_deps
    }

    pub fn add_frame_params(&mut self, e_idx: idx::Expr, e_cxts: &cxt::pre::ECxts) {
        for dep_e_idx in e_cxts[e_idx].e_deps().iter().cloned() {
            let param = rust::typ::generic_param::from_id(e_cxts[dep_e_idx].res_typ_id().clone());
            self.own_frame_generics.params.push(param.clone());
            self.ref_frame_generics.params.push(param);
        }
        for coll in e_cxts[e_idx].colls() {
            let param = rust::typ::generic_param::from_id(coll.acc_t_param_id().clone());
            self.own_frame_generics.params.push(param.clone());
            self.ref_frame_generics.params.push(param);
        }
    }
}

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
        e_cxt: &cxt::pre::ECxt,
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
#[derive(Debug, Clone)]
pub struct DFrames {
    frames: idx::DataMap<Option<Frame>>,
}
impl DFrames {
    pub fn new(e_cxt: &cxt::pre::ECxt, variant: &expr::Variant) -> Self {
        let frames = variant
            .data()
            .index_iter()
            .map(|(d_idx, _)| Frame::new(e_cxt, d_idx, variant.data()))
            .collect();
        Self { frames }
    }

    pub fn is_empty(&self) -> bool {
        self.frames.iter().all(Option::is_none)
    }

    pub fn frames(&self) -> &idx::DataMap<Option<Frame>> {
        &self.frames
    }
}

#[derive(Debug, Clone)]
pub struct VFrames {
    id: rust::Id,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,
    frames: idx::VariantMap<DFrames>,
}
impl VFrames {
    pub fn new(
        e_cxt: &cxt::pre::ECxt,
        expr: &expr::Expr,
        own_generics: rust::Generics,
        ref_generics: rust::Generics,
    ) -> Self {
        let id = gen::frame::typ_id(expr.id());
        let frames = expr
            .variants()
            .iter()
            .map(|variant| DFrames::new(e_cxt, variant))
            .collect();
        Self {
            id,
            own_generics,
            ref_generics,
            frames,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.frames.iter().all(DFrames::is_empty)
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }

    pub fn generics(&self, is_own: IsOwn) -> &rust::Generics {
        if is_own {
            &self.own_generics
        } else {
            &self.ref_generics
        }
    }

    pub fn frames(&self) -> impl Iterator<Item = &Frame> {
        self.frames
            .iter()
            .map(|sub| sub.frames().iter().filter_map(|opt| opt.as_ref()))
            .flatten()
    }

    pub fn frame_sink_arg(&self, is_own: IsOwn) -> rust::Typ {
        let generics = self.generics(is_own);
        let typs = generics
            .lifetimes()
            .map(|lt_def| {
                let lt = lt_def.lifetime.clone();
                let typ: rust::Typ = syn::parse_quote!(& #lt ());
                typ
            })
            .chain(
                generics
                    .type_params()
                    .map(|typ_param| rust::typ::plain(typ_param.ident.clone(), None)),
            );
        rust::typ::tuple(typs)
    }
}

#[derive(Debug, Clone)]
pub struct ECxt {
    pre: pre::ECxt,
    /// Dependency fixed point.
    ///
    /// Always includes self.
    fp_e_deps: Set<idx::Expr>,
    frames: VFrames,
}
implement! {
    impl ECxt {
        Deref<pre::ECxt>, DerefMut {
            field: pre
        }
    }
}

impl ECxt {
    pub fn new(
        pre: pre::ECxt,
        ExprDeps {
            fp_e_deps,
            own_frame_generics,
            ref_frame_generics,
        }: ExprDeps,
        expr: &expr::Expr,
    ) -> Self {
        let frames = VFrames::new(&pre, expr, own_frame_generics, ref_frame_generics);
        Self {
            pre,
            fp_e_deps,
            frames,
        }
    }

    pub fn fp_e_deps(&self) -> &Set<idx::Expr> {
        &self.fp_e_deps
    }
}

/// # Frame-related stuff
impl ECxt {
    pub fn has_frames(&self) -> bool {
        !self.frames.is_empty()
    }
    pub fn frames(&self) -> Option<&VFrames> {
        if self.has_frames() {
            Some(&self.frames)
        } else {
            None
        }
    }
    pub fn frame_generics(&self, is_own: IsOwn) -> &rust::Generics {
        self.frames.generics(is_own)
    }

    pub fn frame_enum_tokens(&self, is_own: IsOwn) -> TokenStream {
        let frames = if let Some(frames) = self.frames() {
            frames
        } else {
            return quote! {};
        };

        let frame_id = frames.id();
        let generics = frames.generics(is_own);

        let (frame_generics, _, where_clause) = generics.split_for_impl();
        let variants = frames.frames().map(|frame| frame.def(is_own));
        let sink_arg = frames.frame_sink_arg(is_own);
        let sink = rust::typ::lib::sink(sink_arg);
        let sink_variant_id = gen::frame::sink_variant_id();

        quote! {
            pub enum #frame_id #frame_generics #where_clause {
                #(#variants ,)*
                #sink_variant_id(#sink),
            }
        }
    }
}
