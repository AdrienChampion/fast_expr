//! Parsing context.

prelude! {}

pub mod frame_enum;
pub mod zip_struct;

pub mod pre;
pub mod with_frames;

pub use self::{frame_enum::FrameEnum, zip_struct::ZipStruct};

#[derive(Debug, Clone)]
pub struct Spec {
    spec: rust::Trait,
}

impl Spec {
    pub fn new(spec: rust::Trait) -> Res<Self> {
        check::spec_trait(&spec)?;
        Ok(Self { spec })
    }

    pub fn id(&self) -> &rust::Id {
        &self.spec.ident
    }
    pub fn generics(&self) -> &rust::Generics {
        &self.spec.generics
    }

    #[inline]
    pub fn log(&self, _pref: impl Display + Copy) {
        logln!(
            "{}spec {} {}",
            _pref,
            self.id(),
            self.generics().to_token_stream()
        )
    }
}

pub type PreCxt = Cxt<pre::ECxt>;
pub type FrameCxt = Cxt<with_frames::ECxt>;

#[derive(Debug, Clone)]
pub struct Cxt<ECxt> {
    /// Specification traits.
    specs: Map<rust::Id, Spec>,

    /// Maps expression identifiers to their indices.
    expr_id_map: Map<rust::Id, idx::Expr>,
    /// Maps expression indices to their context.
    ///
    /// The normal way to access this out of this module is by indexing the context:
    /// `cxt[expr_idx]`.
    exprs: idx::ExprMap<ECxt>,
}
implement! {
    impl(ECxt) Cxt<ECxt> {
        Index<idx::Expr, ECxt> {
            |self, idx| &self.exprs[idx]
        }
    }
}

impl Cxt<pre::ECxt> {
    fn _new() -> Self {
        Self {
            specs: Map::new(),
            expr_id_map: Map::new(),
            exprs: idx::ExprMap::new(),
        }
    }
    fn push_spec(&mut self, spec: rust::Trait) -> Res<()> {
        let new_span = spec.ident.span();
        let spec = Spec::new(spec.clone())?;

        let prev = self.specs.insert(spec.id().clone(), spec);
        if let Some(prev) = prev {
            let first_ident = prev.id();
            bail!(
                on(first_ident, "specification trait `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here"),
            )
        }

        Ok(())
    }
    fn push_expr(&mut self, expr: rust::Enum) -> Res<idx::Expr> {
        let new_span = expr.ident.span();
        let e_idx = self.exprs.next_index();

        let prev = self.expr_id_map.insert(expr.ident.clone(), e_idx);
        if let Some(prev_idx) = prev {
            let first_ident = self[prev_idx].id();
            bail!(
                on(first_ident, "expression enum `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here")
            )
        }

        let _e_idx = self.exprs.push(pre::ECxt::new(self, e_idx, expr)?);
        debug_assert_eq!(e_idx, _e_idx);

        Ok(e_idx)
    }

    /// Constructor from the frontend structure.
    ///
    /// **Warning**: the result of this function is not a fully functional context. It needs to be
    /// [`finalize`]d.
    ///
    /// [`finalize`]: #method.finalize
    fn new(front::Top { specs, exprs }: front::Top) -> Res<Self> {
        let mut slf = Self::_new();

        for spec in specs {
            slf.push_spec(spec)?
        }
        for expr in &exprs {
            let _ = slf.push_expr(expr.clone())?;
        }

        Ok(slf)
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        log! {{
            for spec in self.specs.values() {
                spec.log(_pref)
            }
            for ecxt in &self.exprs {
                ecxt.log(_pref)
            }
        }}
    }
}

impl Cxt<pre::ECxt> {
    pub fn register_expr_data(&mut self, e_idx: idx::Expr, mentions: idx::Expr) {
        let _is_new = self.exprs[e_idx].add_dep(mentions);
    }
    pub fn register_expr_coll_data(
        &mut self,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        d_id: Option<&rust::Id>,
        mentions: idx::Expr,
    ) -> idx::Coll {
        self.register_expr_data(e_idx, mentions);
        let c_idx = self[e_idx].colls().next_index();
        let coll = CollCxt::new(
            e_idx,
            self[e_idx].id(),
            v_idx,
            self[e_idx].v_id(v_idx),
            d_idx,
            d_id,
            c_idx,
        );
        let _c_idx = self.exprs[e_idx].push_coll(coll);
        debug_assert_eq!(c_idx, _c_idx);
        c_idx
    }

    pub fn generate_frames(self, exprs: &expr::Exprs) -> Cxt<with_frames::ECxt> {
        let deps = pre::ECxt::generate_deps(&self, exprs);

        debug_assert_eq!(self.exprs.len(), deps.len());

        let exprs = self
            .exprs
            .into_index_iter()
            .zip(deps.into_index_iter())
            .map(|((e_idx, pre_e_cxt), (_e_idx, deps))| {
                assert_eq!(e_idx, _e_idx);
                with_frames::ECxt::new(pre_e_cxt, deps, &exprs[e_idx])
            })
            .collect();

        Cxt {
            specs: self.specs,
            expr_id_map: self.expr_id_map,
            exprs,
        }
    }
}

impl<ECxt> Cxt<ECxt> {
    /// Retrieves the context of a expression, if any.
    pub fn get_expr(&self, id: &rust::Id) -> Option<&ECxt> {
        self.expr_id_map.get(id).map(|idx| &self[*idx])
    }

    pub fn exprs(&self) -> &idx::ExprMap<ECxt> {
        &self.exprs
    }

    /// Retrieves the specification of an identifier, if any.
    pub fn get_spec(&self, id: &rust::Id) -> Option<&Spec> {
        self.specs.get(id)
    }
}

// #[derive(Debug, Clone)]
// pub struct ECxt {
//     /// Expression index.
//     e_idx: idx::Expr,

//     /// Result type identifier.
//     res_typ_id: rust::Id,
//     /// Result type.
//     res_typ: rust::Typ,

//     /// Expression types mentioned by this expression's definition.
//     ///
//     /// Includes self if self-recursive.
//     ///
//     /// **Only valid after finalization.**
//     e_deps: Set<idx::Expr>,

//     /// Dependency fixed point.
//     ///
//     /// Always includes self.
//     ///
//     /// **Only valid after finalization.**
//     fp_e_deps: Set<idx::Expr>,

//     /// Collection contexts.
//     ///
//     /// **Only valid after finalization.**
//     colls: idx::CollMap<CollCxt>,

//     /// Generics associated with this expression.
//     generics: Option<rust::Generics>,

//     /// Generics of the frame enum for this expression.
//     ///
//     /// **Only valid after finalization.**
//     own_frame_generics: rust::Generics,
//     /// Same as `own_frame_generics`, but with the expression lifetime added as the first parameter.
//     ///
//     /// **Only valid after finalization.**
//     ref_frame_generics: rust::Generics,

//     /// Type parameters introduced by `self.generics()`.
//     top_t_params: rust::GenericArgs,

//     /// Frame-type identifier.
//     frame_typ_id: rust::Id,

//     /// Zip-struct info.
//     zip_struct: Option<ZipStruct>,

//     /// Expression definition from the frontend.
//     def: rust::Enum,
// }

// impl ECxt {
//     /// Constructor.
//     ///
//     /// **Warning**: the result of this function is not a fully functional context. It needs to be
//     /// [`finalize`]d.
//     ///
//     /// [`finalize`]: #method.finalize
//     fn new(cxt: &Cxt, e_idx: idx::Expr, def: rust::Enum) -> Res<Self> {
//         let res_typ_id = gen::typ::res(&def.ident);
//         let res_typ = rust::typ::plain(res_typ_id.clone(), None);

//         let generics = {
//             let mut params = def.generics.params.iter();
//             match (params.next(), params.next()) {
//                 (Some(rust::GenericParam::Type(maybe_spec)), None) => {
//                     if let Some(spec) = cxt.get_spec(&maybe_spec.ident) {
//                         Some(spec.generics().clone())
//                     } else {
//                         None
//                     }
//                 }
//                 _ => None,
//             }
//         };

//         let own_frame_generics = generics.as_ref().unwrap_or(&def.generics).clone();
//         let ref_frame_generics = {
//             use syn::*;

//             let mut generics = own_frame_generics.clone();
//             let params = std::mem::replace(&mut generics.params, punctuated::Punctuated::new());

//             let expr_lt = GenericParam::Lifetime(LifetimeDef {
//                 attrs: vec![],
//                 lifetime: gen::lifetime::expr(),
//                 colon_token: None,
//                 bounds: punctuated::Punctuated::new(),
//             });

//             generics.params.push(expr_lt);
//             generics.params.extend(params);

//             generics
//         };

//         let top_t_params = {
//             let generics = generics.as_ref().unwrap_or(&def.generics);
//             generics
//                 .params
//                 .iter()
//                 .map(|param| {
//                     use rust::{GenericArg, GenericParam::*};
//                     match param {
//                         Type(typ) => {
//                             let typ = &typ.ident;
//                             GenericArg::Type(syn::parse_quote!(#typ))
//                         }
//                         Lifetime(lt) => GenericArg::Lifetime(lt.lifetime.clone()),
//                         Const(cst) => {
//                             let cst = &cst.ident;
//                             GenericArg::Const(syn::parse_quote!(#cst))
//                         }
//                     }
//                 })
//                 .collect()
//         };

//         let e_deps = Set::new();
//         let fp_e_deps = Set::new();

//         let frame_typ_id = gen::frame::typ_id(&def.ident);

//         let colls = idx::CollMap::new();

//         Ok(Self {
//             e_idx,
//             res_typ_id,
//             res_typ,

//             e_deps,
//             fp_e_deps,
//             colls,

//             generics,
//             ref_frame_generics,
//             own_frame_generics,

//             top_t_params,

//             frame_typ_id,

//             zip_struct: None,

//             def,
//         })
//     }

//     fn finalize(selves: &mut idx::ExprMap<Self>, exprs: &idx::ExprMap<expr::Expr>) -> Res<()> {
//         let mut e_idx;

//         macro_rules! e_iter {
//             ($($action:tt)*) => {{
//                 e_idx = idx::Expr::zero();
//                 while e_idx < selves.len() {
//                     { $($action)* }

//                     e_idx.inc()
//                 }
//             }};
//         }

//         // Compute dependency fixed points.
//         let mut known;
//         let mut todo = vec![];
//         logln!("computing fp-s");
//         e_iter! {
//             known = Set::new();
//             debug_assert!(todo.is_empty());

//             logln!("- {}", selves[e_idx].id());

//             let _is_new = selves[e_idx].fp_e_deps.insert(e_idx);
//             let _is_new = known.insert(e_idx);
//             debug_assert!(_is_new);

//             todo.extend(selves[e_idx].e_deps.iter().cloned());

//             logln!("  processing {} deps", todo.len());

//             'fp: loop {
//                 let dep_e_idx = if let Some(e_idx) = todo.pop() {
//                     e_idx
//                 } else {
//                     todo.clear();
//                     break 'fp;
//                 };

//                 let _ = selves[e_idx].fp_e_deps.insert(dep_e_idx);
//                 let is_new = known.insert(dep_e_idx);
//                 if !is_new || dep_e_idx == e_idx {
//                     continue 'fp;
//                 }

//                 // Alright, we're about to do some unsafe stuff. Nothing actually unsafe is going on
//                 // as long as THIS assert HOLDS.
//                 assert_ne!(dep_e_idx, e_idx);
//                 // Going to raw pointers here, because we need to borrow DIFFERENT CELLS of
//                 // `selves` as mutable and immutable at the same time.
//                 let slf = {
//                     let tmp: *mut Self = &mut selves[e_idx];
//                     unsafe {&mut *tmp}
//                 };
//                 let dep = {
//                     let tmp: *const Self = & selves[dep_e_idx];
//                     unsafe {&*tmp}
//                 };

//                 // Have we already computed the fixed point of `dep`?
//                 if dep_e_idx < e_idx {
//                     // Yeah, just merge it in `slf`.
//                     known.extend(dep.fp_e_deps.iter().cloned());
//                     slf.fp_e_deps.extend(dep.fp_e_deps.iter().cloned());
//                 } else {
//                     // No, add its dependencies as todos.
//                     for sub_dep_e_idx in dep.e_deps.iter().cloned() {
//                         let is_new = slf.fp_e_deps.insert(sub_dep_e_idx);
//                         if is_new {
//                             todo.push(sub_dep_e_idx)
//                         }
//                     }
//                 }
//             }
//         }

//         logln!("fixed points:");
//         e_iter! {
//             logln!("- {}:", selves[e_idx].id());
//             for idx in selves[e_idx].fp_e_deps.iter().cloned() {
//                 logln!("  {}", selves[idx].id())
//             }
//         }

//         // Populate frame type parameters.
//         e_iter! {
//             let deps = selves[e_idx].e_deps.clone();

//             for dep_e_idx in deps {
//                 let ident = gen::typ::res(exprs[dep_e_idx].id());

//                 let typ_param = rust::typ::param::from_id(ident);

//                 selves[e_idx]
//                     .own_frame_generics
//                     .params
//                     .push(syn::GenericParam::Type(typ_param.clone()));
//                 selves[e_idx]
//                     .ref_frame_generics
//                     .params
//                     .push(syn::GenericParam::Type(typ_param));

//                 if dep_e_idx == e_idx {
//                     let mut c_idx = idx::Coll::zero();
//                     while c_idx < selves[dep_e_idx].colls().len() {
//                         let typ_param = rust::typ::param::from_id(
//                             selves[dep_e_idx].colls()[c_idx].acc_t_param_id().clone(),
//                         );
//                         selves[e_idx]
//                             .own_frame_generics
//                             .params
//                             .push(syn::GenericParam::Type(typ_param.clone()));
//                         selves[e_idx]
//                             .ref_frame_generics
//                             .params
//                             .push(syn::GenericParam::Type(typ_param));

//                         c_idx.inc();
//                     }
//                 }
//             }
//         }

//         // Construct zip-struct info.
//         e_iter! {
//             debug_assert!(selves[e_idx].zip_struct.is_none());
//             selves[e_idx].zip_struct = Some(ZipStruct::new(e_idx, selves))
//         }

//         Ok(())
//     }

//     /// Index accessor.
//     pub fn e_idx(&self) -> idx::Expr {
//         self.e_idx
//     }

//     /// Identifier accessor.
//     pub fn id(&self) -> &rust::Id {
//         &self.def.ident
//     }
//     /// Id of a variant.
//     pub fn v_id(&self, v_idx: idx::Variant) -> &rust::Id {
//         &self.def.variants[*v_idx].ident
//     }

//     /// Result type identifier.
//     pub fn res_typ_id(&self) -> &rust::Id {
//         &self.res_typ_id
//     }
//     /// Result type.
//     pub fn res_typ(&self) -> &rust::Typ {
//         &self.res_typ
//     }
//     /// Frame-type identifier.
//     pub fn frame_typ_id(&self) -> &rust::Id {
//         &self.frame_typ_id
//     }

//     /// Collection contexts accessor.
//     pub fn colls(&self) -> &idx::CollMap<CollCxt> {
//         &self.colls
//     }

//     /// Expression enum definition.
//     pub fn def(&self) -> &rust::Enum {
//         &self.def
//     }

//     /// Generics accessor.
//     pub fn generics(&self) -> &rust::Generics {
//         self.generics.as_ref().unwrap_or(&self.def.generics)
//     }
//     /// Types introduced in the generics, as arguments.
//     pub fn top_t_params(&self) -> &rust::GenericArgs {
//         &self.top_t_params
//     }

//     pub fn log(&self, _pref: impl Display + Copy) {
//         logln!(
//             "{}expr({}) {}<{}>",
//             _pref,
//             self.e_idx(),
//             self.id(),
//             self.top_t_params
//                 .iter()
//                 .fold(String::new(), |acc, t| format!(
//                     "{}{}{},",
//                     acc,
//                     if acc.is_empty() { "" } else { " " },
//                     t.to_token_stream(),
//                 )),
//         )
//     }

//     pub fn zip_struct(&self) -> &ZipStruct {
//         self.zip_struct
//             .as_ref()
//             .unwrap_or_else(|| panic!("trying to access ECxt::zip_struct before finalization"))
//     }

//     fn add_dep(&mut self, e_idx: idx::Expr) -> bool {
//         self.e_deps.insert(e_idx)
//     }
// }

// impl ECxt {
//     pub fn frame_generics(&self, is_own: IsOwn) -> &rust::Generics {
//         if is_own {
//             &self.own_frame_generics
//         } else {
//             &self.ref_frame_generics
//         }
//     }

//     pub fn frame_sink_arg(&self, cxt: &Cxt, is_own: IsOwn) -> rust::Typ {
//         let typs = self
//             .e_deps
//             .iter()
//             .cloned()
//             .map(|e_idx| cxt[e_idx].res_typ())
//             .cloned();
//         let typ = rust::typ::tuple(typs);

//         if is_own {
//             typ
//         } else {
//             rust::typ::reference(Some(gen::lifetime::expr()), typ)
//         }
//     }
// }

// impl ECxt {
//     pub fn zip_struct_generics(&self, is_own: IsOwn) -> rust::Generics {
//         let mut gen = self.frame_generics(is_own).clone();

//         let e_id = self.id();
//         let lib_path = gen::lib_path();
//         let zip_param = gen::typ::param::zip();
//         gen.params.push(syn::parse_quote!(#zip_param));
//         let wc = gen.where_clause.get_or_insert_with(|| syn::WhereClause {
//             where_token: syn::token::Where::default(),
//             predicates: syn::punctuated::Punctuated::new(),
//         });

//         let zipper_trait = gen::trai::lib::zipper();
//         let (_, params, _) = self.generics().split_for_impl();
//         wc.predicates
//             .push(syn::parse_quote!(#zip_param: #lib_path :: #zipper_trait<#e_id #params>));

//         gen
//     }
// }

#[derive(Debug, Clone)]
pub struct CollCxt {
    /// Variant the collection is for.
    v_idx: idx::Variant,
    /// Data the collection is for.
    d_idx: idx::Data,
    /// Index of this collection.
    c_idx: idx::Coll,
    /// Accumulator type parameter identifier.
    acc_t_param_id: rust::Id,
    /// Accumulator type parameter.
    acc_t_param: rust::Typ,
}
impl CollCxt {
    pub fn new(
        _e_idx: idx::Expr,
        e_id: &rust::Id,
        v_idx: idx::Variant,
        v_id: &rust::Id,
        d_idx: idx::Data,
        d_id: Option<&rust::Id>,
        c_idx: idx::Coll,
    ) -> Self {
        let acc_t_param_id = gen::typ::acc(
            match d_id.map(|id| rust::SnakeId::try_from(id.clone()).and_then(|id| id.to_camel())) {
                Some(Ok(id)) => format!("{}{}{}", e_id, v_id, id),
                None | Some(Err(_)) => format!("{}{}{}", e_id, v_id, d_idx),
            },
        );
        let acc_t_param = rust::typ::plain(acc_t_param_id.clone(), None);
        Self {
            v_idx,
            d_idx,
            c_idx,
            acc_t_param_id,
            acc_t_param,
        }
    }

    pub fn acc_t_param(&self) -> &rust::Typ {
        &self.acc_t_param
    }
    pub fn acc_t_param_id(&self) -> &rust::Id {
        &self.acc_t_param_id
    }
}

#[derive(Debug, Clone)]
pub struct Top<ECxt> {
    pub cxt: Cxt<ECxt>,
    pub exprs: idx::ExprMap<expr::Expr>,
}
impl Top<pre::ECxt> {
    pub fn new(top: front::Top) -> Res<Self> {
        let mut cxt = Cxt::new(top)?;
        let exprs = {
            let mut exprs = idx::ExprMap::with_capacity(cxt.exprs.len());
            let mut e_idx = idx::Expr::zero();
            while e_idx < cxt.exprs.len() {
                let expr = expr::Expr::from_front(&mut cxt, e_idx)?;
                let _e_idx = exprs.push(expr);
                debug_assert_eq!(e_idx, _e_idx);
                e_idx.inc();
            }
            exprs
        };
        Ok(Self { cxt, exprs })
    }

    pub fn log(&self, pref: &str) {
        let sub_pref = &format!("{}    ", pref);
        logln!("cxt {{");
        self.cxt.log(sub_pref);
        logln!("}}");
        logln!("exprs {{");
        for expr in &self.exprs {
            expr.log(sub_pref);
        }
        logln!("}}");
    }

    pub fn generate_frames(self) -> Top<with_frames::ECxt> {
        let exprs = self.exprs;
        let cxt = self.cxt.generate_frames(&exprs);
        Top { cxt, exprs }
    }
}

impl Top<with_frames::ECxt> {
    pub fn to_expr_enum_tokens(&self, stream: &mut TokenStream) {
        for expr in &self.exprs {
            expr.to_expr_enum_tokens(stream)
        }
    }

    pub fn to_zip_mod_tokens_for(&self, stream: &mut TokenStream, is_own: IsOwn) {
        let (zip_doc, zip_mod) = (gen::doc::module::zip(is_own), gen::module::zip(is_own));

        let zip_tokens = self
            .cxt
            .exprs()
            .iter()
            .map(|e_cxt| e_cxt.frame_enum_tokens(is_own));

        stream.extend(quote! {
            #[doc = #zip_doc]
            pub mod #zip_mod {
                use super::*;

                #(#zip_tokens)*
            }
        })
    }

    pub fn to_zip_mods_tokens(&self, stream: &mut TokenStream) {
        self.to_zip_mod_tokens_for(stream, true);
        // self.to_zip_mod_tokens_for(stream, false);
    }
}

impl ToTokens for Top<with_frames::ECxt> {
    fn to_tokens(&self, stream: &mut TokenStream) {
        self.to_expr_enum_tokens(stream);

        self.to_zip_mods_tokens(stream);
    }
}
