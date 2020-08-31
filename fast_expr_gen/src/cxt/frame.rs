//! Contexts with frame information.

prelude! {}

use cxt::pre;

pub type Infos = idx::ExprMap<Info>;
#[derive(Debug, Clone)]
pub struct Info {
    fp_e_deps: Set<idx::Expr>,
    frames: VFrames,
}
#[derive(Debug, Clone)]
pub struct InfoBuilder {
    fp_e_deps: Set<idx::Expr>,
    own_frame_generics: rust::Generics,
    ref_frame_generics: rust::Generics,
}
impl InfoBuilder {
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

    pub fn build(self, cxt: &cxt::PreCxt, e_cxt: &cxt::pre::ECxt, expr: &expr::Expr) -> Info {
        let frames = VFrames::new(
            cxt,
            e_cxt,
            &expr,
            self.own_frame_generics,
            self.ref_frame_generics,
        );
        Info {
            fp_e_deps: self.fp_e_deps,
            frames,
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

    id: rust::Id,

    is_struct_like: bool,
    own_fields: Vec<rust::Id>,
    ref_fields: Vec<rust::Id>,

    own_def: rust::Variant,
    ref_def: rust::Variant,
}

impl Frame {
    pub fn new(
        cxt: &cxt::PreCxt,
        e_cxt: &cxt::pre::ECxt,
        pivot_d_idx: idx::Data,
        data: &idx::DataMap<expr::Data>,
    ) -> Option<Self> {
        let pivot = &data[pivot_d_idx];

        if !pivot.needs_frame() {
            return None;
        }

        let (e_idx, v_idx) = (pivot.e_idx(), pivot.v_idx());
        let id = gen::frame::variant_id(e_cxt.v_id(v_idx), pivot_d_idx, pivot.d_id());

        let is_struct_like = pivot.d_id().is_some();
        let mut own_fields = vec![];
        let mut ref_fields = vec![];

        let (own_variant_fields, ref_variant_fields) = {
            let mut own_variant_fields = syn::punctuated::Punctuated::new();
            let mut ref_variant_fields = syn::punctuated::Punctuated::new();

            for (curr_d_idx, curr_data) in data.index_iter() {
                macro_rules! push_fields {
                    (@ident_colon) => {{
                        curr_data.d_id().map(|id|
                            (Some(id.clone()), Some(syn::token::Colon::default()))
                        ).unwrap_or((None, None))
                    }};

                    (@own $own_typ:expr) => {{
                        own_fields.push(curr_data.param_id().clone());
                        let (ident, colon_token) = push_fields!(@ident_colon);
                        own_variant_fields.push(rust::Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            ident: ident,
                            colon_token: colon_token,
                            ty: $own_typ,
                        })
                    }};
                    (@ref $ref_typ:expr) => {{
                        ref_fields.push(curr_data.param_id().clone());
                        let (ident, colon_token) = push_fields!(@ident_colon);
                        ref_variant_fields.push(rust::Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            ident: ident,
                            colon_token: colon_token,
                            ty: $ref_typ,
                        })
                    }};

                    (opt $($mk_typ_fn:tt)*) => {{
                        if let Some(own_typ) = $($mk_typ_fn)*(cxt, true) {
                            push_fields!(@own own_typ)
                        }
                        if let Some(ref_typ) = $($mk_typ_fn)*(cxt, false) {
                            push_fields!(@ref ref_typ)
                        }
                    }};
                    ($($mk_typ_fn:tt)*) => {{
                        push_fields!(@own $($mk_typ_fn)*(cxt, true));
                        push_fields!(@ref $($mk_typ_fn)*(cxt, false));
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

            if is_struct_like {
                (
                    syn::Fields::Named(syn::FieldsNamed {
                        brace_token: syn::token::Brace::default(),
                        named: own_variant_fields,
                    }),
                    syn::Fields::Named(syn::FieldsNamed {
                        brace_token: syn::token::Brace::default(),
                        named: ref_variant_fields,
                    }),
                )
            } else {
                (
                    syn::Fields::Unnamed(syn::FieldsUnnamed {
                        paren_token: syn::token::Paren::default(),
                        unnamed: own_variant_fields,
                    }),
                    syn::Fields::Unnamed(syn::FieldsUnnamed {
                        paren_token: syn::token::Paren::default(),
                        unnamed: ref_variant_fields,
                    }),
                )
            }
        };

        let own_def = rust::Variant {
            attrs: vec![],
            ident: id.clone(),
            fields: own_variant_fields,
            discriminant: None,
        };
        let ref_def = rust::Variant {
            attrs: vec![],
            ident: id.clone(),
            fields: ref_variant_fields,
            discriminant: None,
        };

        Some(Self {
            e_idx,
            v_idx,
            d_idx: pivot_d_idx,

            id,

            is_struct_like,
            own_fields,
            ref_fields,

            own_def,
            ref_def,
        })
    }

    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }
    pub fn v_idx(&self) -> idx::Variant {
        self.v_idx
    }
    pub fn d_idx(&self) -> idx::Data {
        self.d_idx
    }

    pub fn def(&self, is_own: IsOwn) -> &rust::Variant {
        if is_own {
            &self.own_def
        } else {
            &self.ref_def
        }
    }

    pub fn to_build_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let fields = if is_own {
            &self.own_fields
        } else {
            &self.ref_fields
        };
        if self.is_struct_like {
            quote!(#id { #(#fields ,)* })
        } else {
            quote!(#id ( #(#fields ,)* ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct DFrames {
    frames: idx::DataMap<Option<Frame>>,
}
impl DFrames {
    pub fn new(cxt: &cxt::PreCxt, e_cxt: &cxt::pre::ECxt, variant: &expr::Variant) -> Self {
        let frames = variant
            .data()
            .index_iter()
            .map(|(d_idx, _)| Frame::new(cxt, e_cxt, d_idx, variant.data()))
            .collect();
        Self { frames }
    }

    pub fn is_empty(&self) -> bool {
        self.frames.iter().all(Option::is_none)
    }

    pub fn data_frames(&self) -> &idx::DataMap<Option<Frame>> {
        &self.frames
    }

    pub fn to_build_tokens(&self, d_idx: idx::Data, is_own: IsOwn) -> Option<TokenStream> {
        self.frames[d_idx]
            .as_ref()
            .map(|frame| frame.to_build_tokens(is_own))
    }
}

#[derive(Debug, Clone)]
pub struct VFrames {
    e_idx: idx::Expr,

    id: rust::Id,

    own_generics: rust::Generics,
    ref_generics: rust::Generics,
    frames: idx::VariantMap<DFrames>,
}
impl VFrames {
    pub fn new(
        cxt: &cxt::PreCxt,
        e_cxt: &cxt::pre::ECxt,
        expr: &expr::Expr,
        own_generics: rust::Generics,
        ref_generics: rust::Generics,
    ) -> Self {
        let e_idx = expr.e_idx();
        let id = gen::frame::typ_id(expr.id());
        let frames = expr
            .variants()
            .iter()
            .map(|variant| DFrames::new(cxt, e_cxt, variant))
            .collect();
        Self {
            e_idx,
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
    pub fn plain_typ(&self, is_own: IsOwn) -> rust::Typ {
        let id = &self.id;
        let (_, params, _) = self.generics(is_own).split_for_impl();
        syn::parse_quote!(#id #params)
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
            .map(|sub| sub.data_frames().iter().filter_map(|opt| opt.as_ref()))
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
    pub fn to_build_tokens(
        &self,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        is_own: IsOwn,
    ) -> Option<TokenStream> {
        self.frames[v_idx]
            .to_build_tokens(d_idx, is_own)
            .map(|tokens| {
                let id = &self.id;
                quote!(#id :: #tokens)
            })
    }

    pub fn to_sink_match_case_tokens(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        let frame_id = &self.id;
        let sink_variant = gen::frame::sink_variant_id();
        let empty_id = rust::Id::new("empty", gen::span());
        let match_empty = cxt.lib_gen().sink_match_empty(&empty_id);
        quote!(
            #frame_id :: #sink_variant ( #match_empty ) => match #empty_id {},
        )
    }

    pub fn to_zip_frame_handler_fn_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let e_cxt = &cxt[self.e_idx];
        let id = &e_cxt.self_ids().handle_frame_fun;
        let frame_var = &cxt.zip_ids().frame_var;
        let frame_typ = e_cxt
            .frames()
            .expect("trying to generate frame handler for frame-less expression type")
            .plain_typ(is_own);
        let res_var = &cxt.zip_ids().res_var;
        let res_typ = e_cxt.res_typ_id();
        let out_typ = e_cxt.zip_variant_handler_out_typ(cxt, is_own);

        let do_it = e_cxt.expr().zip_handle_frames(cxt, &res_var, is_own);

        quote! {
            pub fn #id(&mut self, #res_var: #res_typ, #frame_var: #frame_typ) -> #out_typ {
                match #frame_var {
                    #do_it
                }
            }
        }
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
    expr: expr::Expr,
}
implement! {
    impl ECxt {
        Deref<pre::ECxt>, DerefMut {
            field: pre
        }
    }
}

impl ECxt {
    pub fn new(pre: pre::ECxt, Info { fp_e_deps, frames }: Info, expr: expr::Expr) -> Self {
        Self {
            pre,
            fp_e_deps,
            frames,
            expr,
        }
    }

    pub fn fp_e_deps(&self) -> &Set<idx::Expr> {
        &self.fp_e_deps
    }

    pub fn expr(&self) -> &expr::Expr {
        &self.expr
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

/// # Zip-struct generation
impl ECxt {
    pub fn generate_zip_info(cxt: &cxt::FrameCxt) -> cxt::zip::Infos {
        cxt.e_cxts()
            .index_iter()
            .map(|(e_idx, _)| cxt::zip::Info::new(cxt, e_idx))
            .collect()
    }
}
