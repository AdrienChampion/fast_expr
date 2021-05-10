//! Types for the part of the impl-macro input coming from users.

prelude! {}

use super::{impl_kw as kw, map, FnDef, TypeDef};

/// Zip info for all expressions.
#[derive(Debug, Clone)]
pub struct Exprs {
    /// Main (first) expression.
    pub head: Expr,
    /// Other expressions.
    pub tail: Vec<Expr>,
}

impl Exprs {
    /// Iterator over all expressions zippers.
    ///
    /// The main expression zipper is guaranteed to come first.
    pub fn iter(&self) -> impl Iterator<Item = &Expr> + Clone {
        Some(&self.head).into_iter().chain(self.tail.iter())
    }

    /// Main (first) expression identifier.
    pub fn main_e_id(&self) -> &syn::Ident {
        &self.head.e_id
    }

    /// True if we are generating a zipper over owned expressions.
    ///
    /// This function assumes [`Self::check`] already ran.
    ///
    /// [`Self::check`]: ./struct.Exprs.html#method.check
    pub fn is_own(&self) -> IsOwn {
        !self.head.is_ref()
    }

    /// Fails if not consistent.
    ///
    /// # Checks
    ///
    /// - Expression IDs only appear once.
    /// - All expression zippers agree on whether their input expression is borrowed/owned.
    pub fn check(&self) -> Res<()> {
        let mut iter = self.iter();
        while let Some(expr) = iter.next() {
            for other in iter.clone() {
                if expr.e_id == other.e_id {
                    bail!(on &other.e_id =>
                        "found two zippers for `{}` expressions",
                        other.e_id,
                    )
                }
            }
        }
        for expr in &self.tail {
            if self.head.is_ref() != expr.is_ref() {
                bail!(on &expr.e_param_id =>
                    "expected {0} input expression, \
                    because zipper for `{1}` takes {0} expressions",
                    if self.head.is_ref() {
                        "referenced"
                    } else {
                        "owned"
                    },
                    self.head.e_id
                )
            }
        }
        Ok(())
    }

    /// Fails if its expression types are not exactly the ones in `set`.
    pub fn check_mentions_exactly(&self, set: &Set<syn::Ident>) -> Res<()> {
        for id in set {
            if !self.iter().any(|expr| id == &expr.e_id) {
                bail!(on id =>
                    "no zipper found for `{}` expressions", id
                )
            }
        }
        if set.len() != self.tail.len() + 1 {
            for expr in self.iter() {
                if !set.contains(&expr.e_id) {
                    bail!(on &expr.e_id =>
                        "illegal zipper for unexpected `{}` expressions", expr.e_id
                    )
                }
            }
        }
        Ok(())
    }
}

implement! {
    impl Exprs {
        ToTokens, Display {
            |&self, tokens| {
                self.head.to_tokens(tokens);
                for expr in &self.tail {
                    expr.to_tokens(tokens)
                }
            }
        }

        Parse {
            |input| {
                let head = input.parse()?;
                let mut tail = vec![];
                while !input.is_empty() {
                    tail.push(input.parse()?)
                }
                Ok(Self { head, tail })
            }
        }
    }
}

/// Expression zip info.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Zip keyword.
    pub key: kw::zip,
    /// Expression ID.
    pub e_id: syn::Ident,
    /// Expression ID ref indicator, if any.
    pub e_ref: Option<syn::Token![&]>,
    /// Expression type-arguments.
    pub e_targs: syn::PathArguments,
    /// Identifier for the input expression to zip over.
    pub e_param_id: syn::Ident,
    /// Expression type.
    pub e_type: syn::Type,
    /// Result type.
    pub e_res_type: syn::Type,
    /// Expression variants.
    pub variants: Vec<Variant>,
}

impl Expr {
    /// True if the zip is over referenced expressions, not owned ones.
    pub fn is_ref(&self) -> bool {
        self.e_ref.is_some()
    }
}

implement! {
    impl Expr {
        ToTokens, Display {
            |&self, tokens| {
                let Self {
                    key,
                    e_id: _,
                    e_ref,
                    e_targs: _,
                    e_param_id,
                    e_type,
                    e_res_type,
                    variants,
                } = self;
                tokens.extend(quote! {
                    #key(#e_ref #e_param_id : #e_type => #e_res_type) {
                        #(#variants),*
                    }
                })
            }
        }

        Parse {
            |input| {
                let key = input.parse()?;

                let (e_ref, e_param_id, e_type, e_res_type) = {
                    let paren_content;
                    let _paren = syn::parenthesized!(paren_content in input);
                    let e_param_id = paren_content.parse()?;
                    let _: syn::Token![:] = paren_content.parse()?;
                    let e_ref = paren_content.parse()?;
                    let e_type = paren_content.parse()?;
                    let _: syn::Token![=>] = paren_content.parse()?;
                    let e_res_type = paren_content.parse()?;
                    if !paren_content.is_empty() {
                        bail!(paren_content.error("expected closing parenthesis"))
                    }
                    (e_ref, e_param_id, e_type, e_res_type)
                };

                let variants = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);
                    Csv::<Variant>::parse_terminated(&brace_content)?.into_iter().collect()
                };

                let (e_id, e_targs) = {
                    let mut e_type = &e_type;
                    let mut first = true;
                    'find_id: loop {
                        match e_type {
                            rust::Typ::Reference(reference) => {
                                if let Some(e_lt) = reference.lifetime.as_ref() {
                                    bail!(on e_lt =>
                                        "unexpected lifetime annotation"
                                    )
                                }
                                if let Some(mutable) = reference.mutability {
                                    bail!(on mutable =>
                                        "unsupported mutable expression reference"
                                    )
                                }
                                e_type = &*reference.elem;
                            }
                            rust::Typ::Path(path) => {
                                if path.qself.is_some() {
                                    return Err(syn::Error::new_spanned(
                                        path,
                                        "expected expression type path",
                                    ));
                                }
                                if let Some(last_segment) = path.path.segments.iter().next_back() {
                                    break 'find_id (
                                        last_segment.ident.clone(),
                                        last_segment.arguments.clone(),
                                    );
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        path,
                                        "not a legal path to an expression type",
                                    ));
                                }
                            }
                            other => {
                                return Err(syn::Error::new_spanned(
                                    other,
                                    format!(
                                        "expected {}expression type",
                                        if first { "(reference of an) " } else { "" }
                                    ),
                                ))
                            }
                        }
                        first = false;
                    }
                };

                Ok(Self {
                    key,
                    e_id,
                    e_ref,
                    e_targs,
                    e_param_id,
                    e_type,
                    e_res_type,
                    variants,
                })
            }
        }
    }
}

/// Variant zip info.
#[derive(Debug, Clone)]
pub struct Variant {
    /// Variant identifier.
    pub v_id: syn::Ident,
    /// Field bindings.
    pub bindings: Bindings,
    /// Function definitions.
    pub defs: UpAndFolds,
}

impl Variant {
    /// Member of an ident in the variant's bindings.
    pub fn get_member(&self, ident: &syn::Ident) -> Res<syn::Member> {
        self.bindings.get_member(ident)
    }

    /// Updates the receiver and applies its own bindings to a signature.
    pub fn apply_to(
        &self,
        sig: &mut syn::Signature,
        receiver: &syn::Receiver,
        err_span: Span,
    ) -> Res<()> {
        if let Some(syn::FnArg::Receiver(rcvr)) = sig.inputs.iter_mut().next() {
            rcvr.self_token = receiver.self_token.clone()
        }
        self.bindings.apply_to(sig, err_span)
    }

    /// Creates all function/type definitions needed.
    pub fn generate(
        &self,
        expr: &Expr,
        e_map: &map::Expr,
        v_map: &map::Variant,
        receiver: &syn::Receiver,
        is_own: IsOwn,
        mut post_type_def: impl FnMut(TypeDef) -> Res<()>,
        mut post_fn_def: impl FnMut(FnDef) -> Res<()>,
    ) -> Res<()> {
        let go_up_id = &v_map.go_up_id;

        {
            // Work on `go_up`.
            let go_up = &self.defs.go_up;
            let mut err_span = go_up.body.span();
            if let Some(key) = go_up.key.as_ref() {
                err_span = key.span
            }
            let mut sig = e_map.get_trait_fn(go_up_id, is_own)?.sig.clone();
            self.apply_to(&mut sig, receiver, err_span)?;
            let fn_def = FnDef::new(
                self.defs.go_up.key.span(),
                go_up.attrs.clone(),
                sig,
                vec![],
                go_up.body.clone(),
            );
            post_fn_def(fn_def)?;
        }

        // Work on folding, if any.
        for fold in &self.defs.folds {
            let (acc_def, init_def, step_def) =
                fold.generate(expr, self, e_map, v_map, receiver, is_own)?;
            post_type_def(acc_def)?;
            post_fn_def(init_def)?;
            post_fn_def(step_def)?;
        }

        Ok(())
    }
}

implement! {
    impl Variant {
        ToTokens, Display {
            |&self, tokens| {
                let Self { v_id, bindings, defs } = self;
                tokens.extend(quote! {
                    #v_id #bindings => #defs
                })
            }
        }

        Parse {
            |input| {
                let v_id = input.parse()?;
                let bindings = input.parse()?;
                let _: syn::Token![=>] = input.parse()?;
                let defs = input.parse()?;
                Ok(Self { v_id, bindings, defs })
            }
        }
    }
}

/// Variant field bindings.
#[derive(Debug, Clone)]
pub enum Bindings {
    /// Bindings for struct-like variants.
    Struct(Vec<syn::Ident>),
    /// Bindings for tuple-like variants.
    Tuple(Vec<syn::Ident>),
    /// Bindings for unit variants.
    Unit,
}

impl Bindings {
    /// Member of an ident in the variant's bindings.
    pub fn get_member(&self, ident: &syn::Ident) -> Res<syn::Member> {
        match self {
            Self::Struct(_) => Ok(syn::Member::Named(ident.clone())),
            Self::Tuple(idents) => {
                for (idx, id) in idents.iter().enumerate() {
                    if ident == id {
                        return Ok(syn::Member::Unnamed(syn::Index {
                            index: idx as u32,
                            span: ident.span(),
                        }));
                    }
                }
                bail!(on ident =>
                    "cannot retrieve member of `{}` in bindings `{}`",
                    ident,
                    self,
                )
            }
            Self::Unit => bail!(on ident =>
                "cannot retrieve member of `{}` for unit variant",
                ident,
            ),
        }
    }

    /// Applies some binding to a signature.
    pub fn apply_to(&self, sig: &mut syn::Signature, err_span: Span) -> Res<()> {
        match self {
            Bindings::Unit => (),
            Bindings::Struct(idents) => {
                // Idents and signature's inputs should match.
                let self_set: Set<_> = idents.iter().collect();
                let sig_set: Set<_> = sig
                    .inputs
                    .iter()
                    .filter_map(|fn_arg| match fn_arg {
                        syn::FnArg::Typed(pat_type) => match pat_type.pat.as_ref() {
                            syn::Pat::Ident(pat_ident) => Some(&pat_ident.ident),
                            _ => None,
                        },
                        syn::FnArg::Receiver(_) => None,
                    })
                    .collect();

                if idents.len() + 1 != sig.inputs.len() || self_set != sig_set {
                    bail!(@err_span =>
                        "[internal] mismatch between signature {} and bindings {}",
                        sig.to_token_stream(),
                        self,
                    )
                }

                for (idx, input) in sig.inputs.iter_mut().enumerate() {
                    if let syn::FnArg::Typed(pat_type) = input {
                        let ident = &idents[idx - 1];
                        pat_type.pat = Box::new(syn::parse_quote!(#ident));
                    }
                }
            }
            Bindings::Tuple(idents) => {
                'sig_inputs: for (idx, fn_arg) in sig.inputs.iter_mut().enumerate() {
                    match fn_arg {
                        syn::FnArg::Receiver(_) => {
                            if idx > 0 {
                                bail!(@err_span =>
                                    "[internal] unexpected receiver in position {}",
                                    idx,
                                )
                            }
                        }
                        syn::FnArg::Typed(pat_type) => {
                            if idx < 1 {
                                bail!(@err_span =>
                                    "[internal] unexpected typed pattern in position {}",
                                    idx,
                                )
                            }
                            if let Some(ident) = idents.get(idx - 1) {
                                pat_type.pat = Box::new(syn::parse_quote!(#ident));
                            } else {
                                break 'sig_inputs;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

implement! {
    impl Bindings {
        ToTokens, Display {
            |&self, tokens| match self {
                Self::Struct(ids) => tokens.extend(quote! {
                    { #(#ids),* }
                }),
                Self::Tuple(ids) => tokens.extend(quote! {
                    ( #(#ids),* )
                }),
                Self::Unit => (),
            }
        }

        Parse {
            |input| {
                if input.peek(syn::token::Brace) {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);
                    let idents = Csv::parse_terminated(&brace_content)?;
                    Ok(Self::Struct(idents.into_iter().collect()))
                } else if input.peek(syn::token::Paren) {
                    let paren_content;
                    let _paren = syn::parenthesized!(paren_content in input);
                    let idents = Csv::parse_terminated(&paren_content)?;
                    Ok(Self::Tuple(idents.into_iter().collect()))
                } else {
                    Ok(Self::Unit)
                }
            }
        }
    }
}

/// Aggregates a go-up definition and the fold definitions.
#[derive(Debug, Clone)]
pub struct UpAndFolds {
    /// Zip keyword.
    pub key: Option<kw::zip>,
    /// Go-up definition.
    pub go_up: GoUp,
    /// Fold definitions.
    pub folds: Vec<Fold>,
}
impl UpAndFolds {
    /// Constructor from a go-up definition.
    pub fn from_go_up(go_up: impl Into<GoUp>) -> Self {
        let res = Self {
            key: None,
            go_up: go_up.into(),
            folds: vec![],
        };
        res.check();
        res
    }

    /// Checks internal invariants.
    pub fn check(&self) {
        macro_rules! abort {
            {$($blah:tt)*} => {
                panic!("[UpAndFolds] {}", format_args!($($blah)*))
            }
        }
        match (
            self.key.is_some(),
            self.folds.is_empty(),
            self.go_up.key.is_some(),
        ) {
            (true, _, false) => abort!("key is present but go-up key is not"),
            (false, false, _) => abort!("key is not present but there are fold definitions"),
            (false, _, true) => abort!("key is not present but go-up key is"),
            _ => (),
        }
    }
}

implement! {
    impl UpAndFolds {
        ToTokens, Display {
            |&self, tokens| {
                let Self { key, go_up, folds } = self;
                if key.is_none() {
                    go_up.to_tokens(tokens)
                } else {
                    tokens.extend(quote! {
                        #key {
                            #go_up
                            #(, #folds)*
                        }
                    })
                }
            }
        }

        Parse {
            |input| {
                let res = if input.peek(kw::zip) {
                    let key = input.parse()?;

                    let up_or_fold_list = {
                        let brace_content;
                        let _brace = syn::braced!(brace_content in input);
                        Csv::<UpOrFold>::parse_terminated(&brace_content)?
                    };

                    let mut go_up = None;
                    let mut folds = vec![];

                    for up_or_fold in up_or_fold_list {
                        match up_or_fold {
                            UpOrFold::Up(new_go_up) => if go_up.is_none() {
                                    go_up = Some(new_go_up)
                            } else {
                                bail!(on new_go_up =>
                                    "illegal redefinition of `go_up` function"
                                )
                            },

                            UpOrFold::Fold(new_fold) => folds.push(new_fold),
                        }
                    }

                    let go_up = go_up.ok_or_else(|| error!(on key =>
                        "missing `go_up` definition"
                    ))?;

                    Self {
                        key, go_up, folds
                    }
                } else if let Ok(expr) = input.parse() {
                    Self::from_go_up(GoUp::from_expr(expr))
                } else {
                    return Err(input.error(format!("expected `{}` keyword or expression", kw::zip(Span::call_site()).to_token_stream())));
                };

                res.check();
                Ok(res)
            }
        }
    }
}

/// Either a go-up description or a fold description.
///
/// Only used during actual parsing, does not appear in any other type.
#[derive(Debug, Clone)]
pub enum UpOrFold {
    /// Go-up description.
    Up(GoUp),
    /// Fold description.
    Fold(Fold),
}

implement! {
    impl UpOrFold {
        ToTokens, Display {
            |&self, tokens| {
                match self {
                    Self::Up(up) => up.to_tokens(tokens),
                    Self::Fold(fold) => fold.to_tokens(tokens),
                }
            }
        }

        Parse {
            |input| {
                let attrs = syn::Attribute::parse_outer(input)?;
                let lookahead = input.lookahead1();
                let res = if lookahead.peek(kw::go_up) {
                    let mut go_up: GoUp = input.parse()?;
                    go_up.attrs.extend(attrs);
                    Self::Up(go_up)
                } else if lookahead.peek(kw::fold) {
                    if !attrs.is_empty() {
                        return Err(input.error("fold blocks cannot have attributes"));
                    }
                    Self::Fold(input.parse()?)
                } else {
                    bail!(lookahead.error())
                };
                Ok(res)
            }
        }
    }
}

/// Go-up function description.
#[derive(Debug, Clone)]
pub struct GoUp {
    /// Attributes.
    pub attrs: Vec<syn::Attribute>,
    /// Optional go-up keyword.
    pub key: Option<kw::go_up>,
    /// Go-up function body.
    pub body: syn::Expr,
}
impl GoUp {
    /// Constructor.
    pub fn from_expr(body: syn::Expr) -> Self {
        Self {
            attrs: vec![],
            key: None,
            body,
        }
    }
}

implement! {
    impl GoUp {
        ToTokens, Display {
            |&self, tokens| {
                let Self { attrs, key, body } = self;
                let key = key.map(|key| quote!(#key =>));
                tokens.extend(quote! {
                    #(#attrs)*
                    #key #body
                })
            }
        }

        Parse {
            |input| {
                let attrs = vec![];
                let key = input.parse()?;
                let _: syn::Token![=>] = input.parse()?;
                let body = input.parse()?;
                Ok(Self { attrs, key, body })
            }
        }
    }
}

/// Fold description.
#[derive(Debug, Clone)]
pub struct Fold {
    /// Fold keyword.
    pub key: kw::fold,
    /// Data-field the fold is for.
    pub field: syn::Ident,
    /// Target type, same as expression-result type if none.
    pub target: Option<syn::Type>,
    /// Init function description.
    pub init: InitFn,
    /// Step function description.
    pub step: StepFn,
}

impl Fold {
    /// Generates the accumulator type and the init/step functions.
    pub fn generate(
        &self,
        expr: &Expr,
        variant: &Variant,
        e_map: &map::Expr,
        v_map: &map::Variant,
        receiver: &syn::Receiver,
        is_own: IsOwn,
    ) -> Res<(TypeDef, FnDef, FnDef)> {
        let member = variant.get_member(&self.field)?;
        let map::CollDesc {
            acc_type,
            init_fn,
            fold_fn,
            data: _,
        } = v_map.get_coll_desc(&member)?;

        let acc_type = {
            let acc_type_def = self.target.as_ref().unwrap_or(&expr.e_res_type);
            TypeDef::new(vec![], acc_type.clone(), acc_type_def.clone())
        };

        let init_fn = {
            let mut sig = e_map.get_trait_fn(init_fn, is_own)?.sig.clone();
            variant.apply_to(&mut sig, receiver, self.init.key.span)?;
            FnDef::new(
                self.init.key.span(),
                self.init.attrs.clone(),
                sig,
                vec![],
                self.init.body.clone(),
            )
        };

        let step_fn = {
            let mut sig = e_map.get_trait_fn(fold_fn, is_own)?.sig.clone();
            variant.bindings.apply_to(&mut sig, self.init.key.span)?;
            let redef = {
                let (acc, next) = (self.step.acc_pat.clone(), self.step.next_pat.clone());
                super::Redef::new(self.field.to_token_stream(), quote! { (#acc, #next) })
            };
            FnDef::new(
                self.step.key.span(),
                self.step.attrs.clone(),
                sig,
                vec![redef],
                self.step.body.clone(),
            )
        };

        Ok((acc_type, init_fn, step_fn))
    }
}

implement! {
    impl Fold {
        ToTokens, Display {
            |&self, tokens| {
                let Self { key, field, target, init, step } = self;
                tokens.extend(quote! {
                    #key(#field #(=> #target)?) {
                        #init,
                        #step,
                    }
                })
            }
        }

        Parse {
            |input| {
                let key = input.parse()?;

                let (field, target) = {
                    let paren_content;
                    let _paren = syn::parenthesized!(paren_content in input);

                    let field = paren_content.parse()?;
                    let target = if paren_content.is_empty() {
                        None
                    } else {
                        let _: syn::Token![=>] = paren_content.parse()?;
                        Some(paren_content.parse()?)
                    };

                    if !paren_content.is_empty() {
                        bail!(paren_content.error("expected closing parenthesis"));
                    }

                    (field, target)
                };

                let (init, step) = {
                    let brace_content;
                    let _brace = syn::braced!(brace_content in input);

                    let attrs = syn::Attribute::parse_outer(&brace_content)?;
                    let mut init: InitFn = brace_content.parse()?;
                    init.attrs.extend(attrs);

                    let _: syn::Token![,] = brace_content.parse()?;

                    let attrs = syn::Attribute::parse_outer(&brace_content)?;
                    let mut step: StepFn = brace_content.parse()?;
                    step.attrs.extend(attrs);

                    if brace_content.peek(syn::Token![,]) {
                        let _: syn::Token![,] = brace_content.parse()?;
                    }
                    if !brace_content.is_empty() {
                        bail!(brace_content.error("expected closing brace"))
                    }
                    (init, step)
                };

                Ok(Self {
                    key,
                    field,
                    target,
                    init,
                    step,
                })
            }
        }
    }
}

/// Init function description.
#[derive(Debug, Clone)]
pub struct InitFn {
    /// Attributes.
    pub attrs: Vec<syn::Attribute>,
    /// Init keyword.
    pub key: kw::init,
    /// Init function definition.
    pub body: rust::Expr,
}

implement! {
    impl InitFn {
        ToTokens, Display {
            |&self, tokens| {
                let Self {attrs, key, body} = self;
                tokens.extend(quote! {
                    #(#attrs)*
                    #key => #body
                })
            }
        }

        Parse {
            |input| {
                let attrs = vec![];
                let key = input.parse()?;
                let _: syn::Token![=>] = input.parse()?;
                let body = input.parse()?;
                Ok(Self { attrs, key, body })
            }
        }
    }
}

/// Step function description.
#[derive(Debug, Clone)]
pub struct StepFn {
    /// Attributes.
    pub attrs: Vec<syn::Attribute>,
    /// Step keyword.
    pub key: kw::step,
    /// Accumulator binding.
    pub acc_pat: syn::Pat,
    /// Next value binding.
    pub next_pat: syn::Pat,
    /// Step function definition.
    pub body: syn::Expr,
}

implement! {
    impl StepFn {
        ToTokens, Display {
            |&self, tokens| {
                let Self{attrs, key, acc_pat, next_pat, body} = self;
                tokens.extend(quote! {
                    #(#attrs)*
                    #key(#acc_pat, #next_pat) => #body
                })
            }
        }

        Parse {
            |input| {
                let attrs = vec![];

                let key = input.parse()?;

                let (args, paren) = {
                    let paren_content;
                    let paren = syn::parenthesized!(paren_content in input);
                    (Csv::<syn::Pat>::parse_terminated(&paren_content)?, paren)
                };
                let mut args = args.into_iter();

                let (acc_pat, next_pat) = match (args.next(), args.next(), args.next()) {
                    (Some(acc_pat), Some(next_pat), None) => (acc_pat, next_pat),
                    _ => bail!(@paren.span =>
                        "expected exactly two bindings: accumulator, and next value"
                    )
                };

                let _: syn::Token![=>] = input.parse()?;
                let body = input.parse()?;

                Ok(Self {
                    attrs,
                    key,
                    acc_pat,
                    next_pat,
                    body,
                })
            }
        }
    }
}
