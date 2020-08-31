//! Parsing contexts.
//!
//! Structurally, a *context* has three levels:
//!
//! - [`Top`][top] is the top-level structure containing everything;
//! - [`Cxt`][cxt] contains the specification traits, some info about expressions, and most
//!   importantly an [`idx::ExprMap`][expr map] of expression contexts.
//! - expression contexts.
//!
//! The first two levels are polymorphic in the kind of expression context they handle. There are
//! three kinds of expression contexts, corresponding to the three steps towards code generation
//!
//! - [pre](#pre-context): for basic parsing and checks,
//! - [frame](#frame-context): contains frame-related info, and
//! - [zip](#zip-context): contains everything codegen needs, in particular zip/zipper-related info.
//!
//! # Pre Context
//!
//! > Relevant module: [`pre`][pre mod].
//!
//! The first kind of expression context constructed is [`pre::ECxt`][pre ecxt]. It contains very
//! limited information about the expression, its main goal is to construct the [`Expr`][expr]s from
//! the input of the macro and check them. Once this is done, fast_expr cannot fail any more due to
//! bad user input.
//!
//! The associated [`Cxt`][cxt] is [`PreCxt`][pre cxt], and the associated [`Top`][top] is
//! [`PreTop`][pre top]. The latter, when creating itself, creates the [`PreCxt`][pre cxt] and then
//! the [Expr][expr]s, and stores.
//!
//! # Frame Context
//!
//! > Relevant module: [`frame`][frame mod].
//!
//! The second step is to generate frame-related information, which requires inspecting the
//! [`Expr`][expr]s using information from the [`pre::ECxt`][pre cxt]s, to create
//! [`frame::ECxt`][frame ecxt]s. This information is represented by [`frame::Info`][frame info],
//! and constructed by [`pre::ECxt::generate_frame_info`][gen frame info].
//!
//! Once all the frame information for each expression context is generated, the [`PreCxt`][pre cxt]
//! turns itself `into` a [`FrameCxt`][frame cxt] by creating each [`frame::ECxt`][frame ecxt]s from
//! a [`pre::ECxt`][pre ecxt] and the corresponding frame info and [`Expr`][expr]. Note that,
//! contrary to [`pre::ECxt`][pre ecxt], [`frame::ECxt`][frame ecxt]s store the corresponding
//! [`Expr`][expr] structure. Therefore the [`FrameTop`][frame top] resulting from this operation
//! does not contain the [`Expr`][expr]s directly anymore, as they are store in the expression
//! contexts below.
//!
//! # Zip Context
//!
//! > Relevant module: [`zip`][zip mod].
//!
//! The third and last step is to generate zip/zipper-related information, heavily relying on the
//! frame information computed at the previous step. The workflow is similar, we first generate zip
//! information ([`zip::Info`][zip info]) for each [`frame::ECxt`][frame ecxt] using
//! [`frame::ECxt::generate_zip_info`][gen zip info]. Then, the [`FrameCxt`] will turn itself `into`
//! a [`ZipCxt`] by creating each [`zip::ECxt`][zip ecxt] from a [`frame::ECxt`][frame ecxt] and the
//! corresponding zip info.
//!
//! At top-level, the [`ZipTop`][zip top] created stores the [`ZipCxt`][zip cxt] and implements
//! [`quote`]'s [`ToTokens`] which generates the whole code.
//!
//! [pre mod]: ./pre
//! [frame mod]: ./frame
//! [zip mod]: ./zip
//! [top]: ./struct.Top.html
//! [pre top]: ./type.PreTop.html
//! [frame top]: ./type.FrameTop.html
//! [zip top]: ./type.ZipTop.html
//! [cxt]: ./struct.Cxt.html
//! [pre cxt]: ./type.PreCxt.html
//! [frame cxt]: ./type.FrameCxt.html
//! [zip cxt]: ./type.FrameCxt.html
//! [pre ecxt]: ./pre/struct.ECxt.html
//! [frame ecxt]: ./frame/struct.ECxt.html
//! [zip ecxt]: ./zip/struct.ECxt.html
//! [frame info]: ./frame/struct.Info.html
//! [zip info]: ./zip/struct.Info.html
//! [gen frame info]: ./pre/struct.ECxt.html#method.generate_frame_info
//! [gen zip info]: ./frame/struct.ECxt.html#method.generate_zip_info
//! [expr]: ../expr/struct.Expr.html
//! [expr map]: ../prelude/idx/struct.ExprMap.html
//!
//! [`quote`]: https://crates.io/crates/quote
//! [`ToTokens`]: https://docs.rs/quote/1.0.7/quote/trait.ToTokens.html

prelude! {}

pub mod frame;
pub mod pre;
pub mod zip;

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
pub type FrameCxt = Cxt<frame::ECxt>;
pub type ZipCxt = Cxt<zip::ECxt>;

pub trait PreCxtLike {
    fn get_pre_e_cxt(&self, e_idx: idx::Expr) -> &pre::ECxt;
    fn lib_gen(&self) -> &gen::Lib;
}
impl PreCxtLike for PreCxt {
    fn get_pre_e_cxt(&self, e_idx: idx::Expr) -> &pre::ECxt {
        &self[e_idx]
    }
    fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }
}
impl PreCxtLike for FrameCxt {
    fn get_pre_e_cxt(&self, e_idx: idx::Expr) -> &pre::ECxt {
        &self[e_idx]
    }
    fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }
}
impl PreCxtLike for ZipCxt {
    fn get_pre_e_cxt(&self, e_idx: idx::Expr) -> &pre::ECxt {
        &self[e_idx]
    }
    fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }
}

pub trait FrameCxtLike {
    fn get_frame_e_cxt(&self, e_idx: idx::Expr) -> &frame::ECxt;
    fn lib_gen(&self) -> &gen::Lib;
}
impl FrameCxtLike for FrameCxt {
    fn get_frame_e_cxt(&self, e_idx: idx::Expr) -> &frame::ECxt {
        &self[e_idx]
    }
    fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }
}
impl FrameCxtLike for ZipCxt {
    fn get_frame_e_cxt(&self, e_idx: idx::Expr) -> &frame::ECxt {
        &self[e_idx]
    }
    fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }
}

pub type PreTop = Top<PreCxt, expr::Exprs>;
pub type FrameTop = Top<FrameCxt, ()>;
pub type ZipTop = Top<ZipCxt, ()>;

#[derive(Debug, Clone)]
pub struct Cxt<ECxt> {
    /// Configuration.
    conf: conf::Conf,
    /// Handles fast_expr-lib-related codegen.
    lib_gen: gen::Lib,

    /// Specification traits.
    specs: Map<rust::Id, Spec>,

    /// Standard variables for the zipper structure.
    zip_ids: gen::ZipIds,

    /// Maps expression identifiers to their indices.
    expr_id_map: Map<rust::Id, idx::Expr>,
    /// Maps expression indices to their context.
    ///
    /// The normal way to access this out of this module is by indexing the context:
    /// `cxt[expr_idx]`.
    e_cxts: idx::ExprMap<ECxt>,
}
implement! {
    impl(ECxt) Cxt<ECxt> {
        Index<idx::Expr, ECxt> {
            |self, idx| &self.e_cxts[idx]
        }
    }
}

impl<ECxt> Cxt<ECxt> {
    /// Global configuration.
    pub fn conf(&self) -> &conf::Conf {
        &self.conf
    }
    /// Codegen for fast_expr types.
    pub fn lib_gen(&self) -> &gen::Lib {
        &self.lib_gen
    }

    /// Retrieves the context of a expression, if any.
    pub fn get_e_cxt(&self, id: &rust::Id) -> Option<&ECxt> {
        self.expr_id_map.get(id).map(|idx| &self[*idx])
    }

    pub fn zip_ids(&self) -> &gen::ZipIds {
        &self.zip_ids
    }

    pub fn e_cxts(&self) -> &idx::ExprMap<ECxt> {
        &self.e_cxts
    }

    /// Retrieves the specification of an identifier, if any.
    pub fn get_spec(&self, id: &rust::Id) -> Option<&Spec> {
        self.specs.get(id)
    }
}

impl PreCxt {
    fn _new(conf: conf::Conf) -> Self {
        let lib_gen = gen::Lib::new(&conf);
        Self {
            conf,
            lib_gen,
            specs: Map::new(),
            zip_ids: gen::ZipIds::default(),
            expr_id_map: Map::new(),
            e_cxts: idx::ExprMap::new(),
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
    fn push_expr(&mut self, expr: rust::Enum, e_conf: conf::EConf) -> Res<idx::Expr> {
        let new_span = expr.ident.span();
        let e_idx = self.e_cxts.next_index();

        let prev = self.expr_id_map.insert(expr.ident.clone(), e_idx);
        if let Some(prev_idx) = prev {
            let first_ident = self[prev_idx].e_id();
            bail!(
                on(first_ident, "expression enum `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here")
            )
        }

        let _e_idx = self.e_cxts.push(pre::ECxt::new(self, e_idx, expr, e_conf)?);
        debug_assert_eq!(e_idx, _e_idx);

        Ok(e_idx)
    }

    /// Constructor from the frontend structure.
    ///
    /// **Warning**: the result of this function is not a fully functional context. It needs to be
    /// [`finalize`]d.
    ///
    /// [`finalize`]: #method.finalize
    fn new(front::Top { specs, exprs, conf }: front::Top) -> Res<Self> {
        let mut slf = Self::_new(conf);

        for spec in specs {
            slf.push_spec(spec)?
        }
        for (e_conf, expr) in exprs {
            let _ = slf.push_expr(expr, e_conf)?;
        }

        Ok(slf)
    }

    pub fn specs(&self) -> &Map<rust::Id, Spec> {
        &self.specs
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        log! {{
            for spec in self.specs.values() {
                spec.log(_pref)
            }
            for ecxt in &self.e_cxts {
                ecxt.log(_pref)
            }
        }}
    }
}

impl PreCxt {
    pub fn register_expr_data(&mut self, e_idx: idx::Expr, mentions: idx::Expr) {
        let _is_new = self.e_cxts[e_idx].add_dep(mentions);
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
            self[e_idx].e_id(),
            v_idx,
            self[e_idx].v_id(v_idx),
            d_idx,
            d_id,
            c_idx,
        );
        let _c_idx = self.e_cxts[e_idx].push_coll(coll);
        debug_assert_eq!(c_idx, _c_idx);
        c_idx
    }

    pub fn generate_frames(self, exprs: expr::Exprs) -> FrameCxt {
        let frame_info = pre::ECxt::generate_frame_info(&self, &exprs);

        debug_assert_eq!(self.e_cxts.len(), frame_info.len());
        debug_assert_eq!(self.e_cxts.len(), exprs.len());

        let e_cxts = self
            .e_cxts
            .into_index_iter()
            .zip(frame_info.into_index_iter())
            .zip(exprs.into_index_iter())
            .map(
                |(((e_idx, pre_e_cxt), (_e_idx, frame_info)), (__e_idx, expr))| {
                    assert_eq!(e_idx, _e_idx);
                    assert_eq!(e_idx, __e_idx);
                    frame::ECxt::new(pre_e_cxt, frame_info, expr)
                },
            )
            .collect();

        Cxt {
            conf: self.conf,
            lib_gen: self.lib_gen,
            specs: self.specs,
            zip_ids: self.zip_ids,
            expr_id_map: self.expr_id_map,
            e_cxts,
        }
    }
}

impl FrameCxt {
    pub fn generate_zip(self) -> ZipCxt {
        let zip_info = frame::ECxt::generate_zip_info(&self);

        debug_assert_eq!(self.e_cxts.len(), zip_info.len());

        let e_cxts = self
            .e_cxts
            .into_index_iter()
            .zip(zip_info.into_index_iter())
            .map(|((e_idx, frame_e_cxt), (_e_idx, zip_info))| {
                assert_eq!(e_idx, _e_idx);
                zip::ECxt::new(frame_e_cxt, zip_info)
            })
            .collect();

        Cxt {
            conf: self.conf,
            lib_gen: self.lib_gen,
            specs: self.specs,
            zip_ids: self.zip_ids,
            expr_id_map: self.expr_id_map,
            e_cxts,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CollCxt {
    e_idx: idx::Expr,
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
        e_idx: idx::Expr,
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
            e_idx,
            v_idx,
            d_idx,
            c_idx,
            acc_t_param_id,
            acc_t_param,
        }
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
    pub fn c_idx(&self) -> idx::Coll {
        self.c_idx
    }

    pub fn acc_t_param(&self) -> &rust::Typ {
        &self.acc_t_param
    }
    pub fn acc_t_param_id(&self) -> &rust::Id {
        &self.acc_t_param_id
    }
}

#[derive(Debug, Clone)]
pub struct Top<Cxt, Exprs> {
    pub cxt: Cxt,
    pub exprs: Exprs,
}

impl PreTop {
    pub fn new_pre(top: front::Top) -> Res<Self> {
        let mut cxt = Cxt::new(top)?;
        let exprs = {
            let mut exprs = idx::ExprMap::with_capacity(cxt.e_cxts.len());
            let mut e_idx = idx::Expr::zero();
            while e_idx < cxt.e_cxts.len() {
                let expr = expr::Expr::from_front(&mut cxt, e_idx)?;
                let _e_idx = exprs.push(expr);
                debug_assert_eq!(e_idx, _e_idx);
                e_idx.inc();
            }
            exprs
        };
        Ok(Self { cxt, exprs })
    }

    pub fn generate_frames(self) -> Top<FrameCxt, ()> {
        let exprs = self.exprs;
        let cxt = self.cxt.generate_frames(exprs);
        Top { cxt, exprs: () }
    }
}

impl FrameTop {
    pub fn generate_zip_structs(self) -> Top<ZipCxt, ()> {
        let cxt = self.cxt.generate_zip();
        Top { cxt, exprs: () }
    }
}

impl ZipTop {
    pub fn new(top: front::Top) -> Res<Self> {
        Top::new_pre(top).map(|pre_top| pre_top.generate_frames().generate_zip_structs())
    }

    pub fn to_expr_enum_tokens(&self, stream: &mut TokenStream) {
        for e_cxt in self.cxt.e_cxts() {
            e_cxt.expr().to_expr_enum_tokens(stream)
        }
    }

    pub fn to_zip_mod_tokens_for(&self, stream: &mut TokenStream, is_own: IsOwn) {
        let (zip_doc, zip_mod) = (gen::doc::module::zip(is_own), gen::module::zip(is_own));

        let tokens = self
            .cxt
            .e_cxts()
            .iter()
            .map(|e_cxt| e_cxt.zip_mod_tokens(&self.cxt, is_own));

        stream.extend(quote! {
            #[doc = #zip_doc]
            pub mod #zip_mod {
                use super::*;

                #(#tokens)*
            }
        })
    }

    pub fn to_zip_mods_tokens(&self, stream: &mut TokenStream) {
        self.to_zip_mod_tokens_for(stream, true);
        self.to_zip_mod_tokens_for(stream, false);
    }
}

impl ToTokens for ZipTop {
    fn to_tokens(&self, stream: &mut TokenStream) {
        self.to_expr_enum_tokens(stream);

        self.to_zip_mods_tokens(stream);
    }
}

impl ZipTop {
    #[cfg(not(feature = "dbg_log"))]
    pub fn dbg_log_to_file(&self) {}
    #[cfg(feature = "dbg_log")]
    pub fn dbg_log_to_file(&self) {
        use std::{fs::OpenOptions, process::Command};

        let file_path = &format!("fast_expr_log.rs");
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(file_path)
            .unwrap_or_else(|e| panic!("failed to open {:?}: {}", file_path, e));

        write!(file, "{}", self.to_token_stream())
            .unwrap_or_else(|e| panic!("failed to write to {:?}: {}", file_path, e));

        let output = Command::new("rustfmt")
            .arg(file_path)
            .output()
            .unwrap_or_else(|e| panic!("failed to run rustfmt on {:?}: {}", file_path, e));

        if !output.status.success() {
            let stdout = String::from_utf8(output.stdout)
                .unwrap_or_else(|e| panic!("failed to convert rustfmt's stdout to utf8: {}", e));
            let stderr = String::from_utf8(output.stderr)
                .unwrap_or_else(|e| panic!("failed to convert rustfmt's stderr to utf8: {}", e));

            println!("Error while running rustfmt");
            println!("|===| stdout");
            println!("|");
            for line in stdout.lines() {
                println!("| {}", line)
            }
            println!("|");
            println!("|===| stderr");
            println!("|");
            for line in stderr.lines() {
                println!("| {}", line)
            }
            println!("|");
            println!("|===|");

            panic!("rustfmt failed while formatting {:?}", file_path)
        }
    }
}
