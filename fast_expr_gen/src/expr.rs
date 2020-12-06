//! Expression structures.

prelude! {}

pub mod data;
pub mod variant;

pub use self::{data::Data, variant::Variant};

/// A list of variants.
pub type Variants = idx::VariantMap<Variant>;

/// A list of expressions.
pub type Exprs = idx::ExprMap<Expr>;

/// Raw representation of an input expression.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Expression index.
    e_idx: idx::Expr,

    /// Expression generics.
    generics: rust::Generics,

    /// Variant list.
    variants: Variants,
    /// Map from variant names to variant indices.
    variant_map: Map<Ident, idx::Variant>,

    /// Frontend version.
    src: rust::Enum,
}

implement! {
    impl Expr {
        Index<idx::Variant, Variant> {
            |self, v_idx| &self.variants[v_idx]
        }
    }
}

impl Expr {
    /// Constructor from a frontend representation.
    pub fn from_front(cxt: &mut cxt::PreCxt, e_idx: idx::Expr) -> Res<Self> {
        let src = cxt[e_idx].def().clone();
        let mut variants = idx::VariantMap::<Variant>::with_capacity(src.variants.len());
        let mut variant_map = Map::new();

        for variant in &src.variants {
            let v_idx = variants.next_index();
            let variant = Variant::from_front(cxt, e_idx, v_idx, variant)?;

            let prev = variant_map.insert(variant.v_id().clone(), v_idx);
            if let Some(prev_idx) = prev {
                bail!(
                    on(
                        variants[prev_idx].v_id(),
                        "expression enum `{}` is defined multiple times",
                        variant.v_id(),
                    ),
                    on(variant.v_id(), "redefined here")
                )
            }

            let _v_idx = variants.push(variant);
            debug_assert_eq!(v_idx, _v_idx);
        }

        Ok(Self {
            e_idx,

            generics: cxt[e_idx].generics().clone(),

            variants,
            variant_map,

            src,
        })
    }
}

impl Expr {
    /// Expression's identifier..
    pub fn e_id(&self) -> &Ident {
        &self.src.ident
    }
    /// Expression's index.
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }

    /// True if the expression mentions itself directly.
    pub fn is_self_rec(&self) -> bool {
        self.variants.iter().any(|variant| variant.is_self_rec())
    }

    /// Variants accessor.
    pub fn variants(&self) -> &idx::VariantMap<Variant> {
        &self.variants
    }

    /// Pushes a new variant.
    pub fn push_variant(&mut self, variant: Variant) -> Res<idx::Variant> {
        let v_idx = self.variants.next_index();

        let prev = self.variant_map.insert(variant.v_id().clone(), v_idx);
        if let Some(prev_idx) = prev {
            bail!(
                on(
                    self[prev_idx].v_id(),
                    "variant `{}` is defined multiple times",
                    variant.v_id()
                ),
                on(variant.v_id(), "re-defined here"),
            )
        }

        let _v_idx = self.variants.push(variant);
        debug_assert_eq!(v_idx, _v_idx);

        Ok(v_idx)
    }

    /// Generates code for handling the variants in a `Zip` struct.
    ///
    /// Generates a case for each frame variant and handles it by calling the appropriate function.
    pub fn to_handle_frames_cases_tokens(
        &self,
        stream: &mut TokenStream,
        cxt: &cxt::ZipCxt,
        res: &Ident,
        is_own: bool,
    ) {
        let frames = cxt[self.e_idx]
            .frames()
            .expect("trying to build frame handler for frame-less expression type");

        for frame in frames.frames() {
            let v_idx = frame.v_idx();
            let d_idx = frame.d_idx();

            let variant = &self.variants[v_idx];
            let data = &variant.data();

            let frame_deconstruction = frames.to_build_tokens(frame.v_idx(), frame.d_idx(), is_own);

            let handle = data[d_idx].zip_handle_frame(
                cxt,
                res,
                is_own,
                |input| cxt.lib_gen().zip_do_new_go_down(input),
                || {
                    let mut d_idx = d_idx;
                    d_idx.inc();
                    if d_idx < data.len() {
                        variant.to_next_frame_tokens(cxt, is_own, d_idx)
                    } else {
                        variant.to_go_up_call_tokens(cxt)
                    }
                },
            );

            stream.extend(quote! {
                #frame_deconstruction => {
                    #handle
                }
            })
        }

        stream.extend(frames.to_sink_match_case_tokens(cxt));
    }
}

/// # Main codegen functions.
impl Expr {
    /// Generates code for the actual expression enum.
    pub fn to_expr_enum_tokens(&self, stream: &mut TokenStream) {
        stream.append_all(&self.src.attrs);
        self.src.vis.to_tokens(stream);
        self.src.enum_token.to_tokens(stream);
        self.src.ident.to_tokens(stream);
        {
            let (type_generics, _, where_clause) = self.generics.split_for_impl();
            type_generics.to_tokens(stream);
            where_clause.to_tokens(stream);
        }
        self.src.brace_token.surround(stream, |stream| {
            debug_assert_eq!(self.src.variants.len(), self.variants.len());
            let mut v_idx = idx::Variant::zero();
            gen::punct::do_with(&self.src.variants, |punct_opt| {
                self.variants[v_idx].to_expr_variant_tokens(stream);
                v_idx.inc();
                if let Some(punct) = punct_opt {
                    punct.to_tokens(stream)
                }
            })
        })
    }

    /// Iterator over variant constructor paths.
    pub fn to_variant_constructors_tokens<'a>(
        &'a self,
    ) -> impl Iterator<Item = (idx::Variant, TokenStream)> + 'a {
        let id = self.e_id();
        self.variants.index_iter().map(move |(v_idx, variant)| {
            let mut stream = quote!(#id ::);
            variant.to_constructor_tokens(&mut stream);
            (v_idx, stream)
        })
    }
}
