//! Parsing context.

prelude! {}

pub mod frames;
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
pub type FrameCxt = Cxt<frames::ECxt>;
pub type ZipCxt = Cxt<zip::ECxt>;

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
    e_cxts: idx::ExprMap<ECxt>,
}
implement! {
    impl(ECxt) Cxt<ECxt> {
        Index<idx::Expr, ECxt> {
            |self, idx| &self.e_cxts[idx]
        }
    }
}

impl PreCxt {
    fn _new() -> Self {
        Self {
            specs: Map::new(),
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
    fn push_expr(&mut self, expr: rust::Enum) -> Res<idx::Expr> {
        let new_span = expr.ident.span();
        let e_idx = self.e_cxts.next_index();

        let prev = self.expr_id_map.insert(expr.ident.clone(), e_idx);
        if let Some(prev_idx) = prev {
            let first_ident = self[prev_idx].id();
            bail!(
                on(first_ident, "expression enum `{}` is defined multiple times", first_ident),
                @(new_span, "redefined here")
            )
        }

        let _e_idx = self.e_cxts.push(pre::ECxt::new(self, e_idx, expr)?);
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
            self[e_idx].id(),
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
                    frames::ECxt::new(pre_e_cxt, frame_info, expr)
                },
            )
            .collect();

        Cxt {
            specs: self.specs,
            expr_id_map: self.expr_id_map,
            e_cxts,
        }
    }
}

impl FrameCxt {
    pub fn generate_zip(self) -> ZipCxt {
        let zip_info = frames::ECxt::generate_zip_info(&self);

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
            specs: self.specs,
            expr_id_map: self.expr_id_map,
            e_cxts,
        }
    }
}

impl<ECxt> Cxt<ECxt> {
    /// Retrieves the context of a expression, if any.
    pub fn get_e_cxt(&self, id: &rust::Id) -> Option<&ECxt> {
        self.expr_id_map.get(id).map(|idx| &self[*idx])
    }

    pub fn e_cxts(&self) -> &idx::ExprMap<ECxt> {
        &self.e_cxts
    }

    /// Retrieves the specification of an identifier, if any.
    pub fn get_spec(&self, id: &rust::Id) -> Option<&Spec> {
        self.specs.get(id)
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

impl Top<PreCxt, expr::Exprs> {
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

impl Top<FrameCxt, ()> {
    pub fn generate_zip_structs(self) -> Top<ZipCxt, ()> {
        let cxt = self.cxt.generate_zip();
        Top { cxt, exprs: () }
    }
}

impl Top<ZipCxt, ()> {
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

impl ToTokens for Top<ZipCxt, ()> {
    fn to_tokens(&self, stream: &mut TokenStream) {
        self.to_expr_enum_tokens(stream);

        self.to_zip_mods_tokens(stream);
    }
}

impl Top<ZipCxt, ()> {
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
