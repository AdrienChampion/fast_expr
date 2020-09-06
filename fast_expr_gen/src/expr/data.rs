//! Variant data representation.

prelude! {}

pub mod leaf;
pub mod many;
pub mod one;

pub use leaf::Leaf;
pub use many::Many;
pub use one::One;

/// Some variant data.
#[derive(Debug, Clone)]
pub struct Data {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,

    data: DataTyp,

    src: rust::Field,

    param_id: rust::Id,
}

impl Data {
    pub fn from_front(
        cxt: &mut cxt::PreCxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        field: &rust::Field,
    ) -> Res<Self> {
        let src = field.clone();
        let typ = &field.ty;

        let data_typ: DataTyp = self::front::resolve_typ(cxt, typ)?.into_data_typ(
            cxt,
            e_idx,
            v_idx,
            d_idx,
            src.ident.as_ref(),
            typ,
        )?;

        let param_id = gen::fun::param::data_param(
            src.ident
                .as_ref()
                .map(Either::Left)
                .unwrap_or_else(|| Either::Right(d_idx)),
        );

        Ok(Self {
            e_idx,
            v_idx,
            d_idx,

            data: data_typ.into(),
            src,

            param_id,
        })
    }

    pub fn d_id(&self) -> Option<&rust::Id> {
        self.src.ident.as_ref()
    }

    pub fn is_self_rec(&self) -> bool {
        self.data.is_self_rec()
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

    pub fn data(&self) -> &DataTyp {
        &self.data
    }

    pub fn typ(&self) -> &rust::Typ {
        self.data.typ()
    }

    pub fn is_leaf(&self) -> bool {
        self.data.is_leaf()
    }

    pub fn param_id(&self) -> &rust::Id {
        &self.param_id
    }

    pub fn map_rec_exprs(&self, action: impl FnMut(idx::Expr, IsColl) -> Res<()>) -> Res<()> {
        self.data.map_rec_exprs(action)
    }

    pub fn as_many(&self) -> Option<&Many> {
        match &self.data {
            DataTyp::Many(many) => Some(many),
            DataTyp::Leaf(_) | DataTyp::One(_) => None,
        }
    }
    pub fn inner(&self) -> Option<idx::Expr> {
        self.data.inner()
    }
}

impl Data {
    pub fn to_expr_data_tokens(&self, stream: &mut TokenStream) {
        stream.append_all(&self.src.attrs);
        self.src.vis.to_tokens(stream);
        if let Some(ident) = &self.src.ident {
            ident.to_tokens(stream);
            if let Some(token) = self.src.colon_token {
                token.to_tokens(stream)
            } else {
                rust::token::Colon {
                    spans: [rust::Span::mixed_site()],
                }
                .to_tokens(stream)
            }
        }
        self.data.to_expr_data_tokens(stream);
    }

    pub fn zip_handle_frame(
        &self,
        cxt: &cxt::ZipCxt,
        res: &rust::Id,
        is_own: IsOwn,
        frame_expr_pair_do: impl FnOnce(TokenStream) -> TokenStream,
        keep_going: impl FnOnce() -> TokenStream,
    ) -> TokenStream {
        let id = self.param_id();

        match self.data() {
            DataTyp::One(one) if one.is_self_rec() => {
                let keep_going = keep_going();
                quote! {
                    let #id = #res;
                    #keep_going
                }
            }

            DataTyp::Many(many) if many.is_self_rec() => {
                let next_id = rust::Id::new("fast_expr_reserved_next", gen::span());
                let acc_field = cxt.lib_gen().coll_der_acc_field();
                let iter_field = cxt.lib_gen().coll_der_iter_field();

                let fold = {
                    let folder = cxt[many.e_idx()].coll_handlers()[many.c_idx()].folder();
                    let fold = folder.to_call_tokens(cxt, res);
                    let step_field = &cxt.zip_ids().self_step_field();
                    cxt.lib_gen()
                        .zip_do_early_return_if_not_down(quote!(#step_field.#fold))
                };

                let keep_going = keep_going();
                let build_frame = cxt[self.e_idx]
                    .frames()
                    .unwrap_or_else(|| {
                        panic!(
                            "trying to build a frame for an expression type with no frames \
                            {}::{}::{}",
                            cxt[self.e_idx].e_id(),
                            cxt[self.e_idx].expr()[self.v_idx].v_id(),
                            cxt[self.e_idx].expr()[self.v_idx][self.d_idx].param_id(),
                        )
                    })
                    .to_build_tokens(self.v_idx, self.d_idx, is_own)
                    .unwrap_or_else(|| {
                        panic!(
                            "trying to build a frame for some expression data with no frames \
                            {}::{}::{}",
                            cxt[self.e_idx].e_id(),
                            cxt[self.e_idx].expr()[self.v_idx].v_id(),
                            cxt[self.e_idx].expr()[self.v_idx][self.d_idx].param_id(),
                        )
                    });
                let and_then = frame_expr_pair_do(quote! {
                    (#build_frame, #next_id)
                });

                quote! {
                    let mut #id = #id;
                    #id.#acc_field = #fold;

                    if let Some(#next_id) = #id.#iter_field.next() {
                        #and_then
                    } else {
                        let #id = #id.#acc_field;
                        #keep_going
                    }
                }
            }

            DataTyp::Leaf(_) | DataTyp::One(_) | DataTyp::Many(_) => {
                // panic!(
                //     "trying to update frame result for some non-self-rec data \
                //                 {}::{}::{}",
                //     cxt[self.e_idx].id(),
                //     cxt[self.e_idx].expr()[self.v_idx].id(),
                //     cxt[self.e_idx].expr()[self.v_idx][self.d_idx].param_id(),
                // );
                let blah = format!(
                    "{}::{}::{}",
                    cxt[self.e_idx].e_id(),
                    cxt[self.e_idx].expr()[self.v_idx].v_id(),
                    cxt[self.e_idx].expr()[self.v_idx][self.d_idx].param_id(),
                );
                quote!(todo!(#blah))
            }
        }
    }

    pub fn zip_handle_variant_data(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        frame_expr_pair_do: impl FnOnce(TokenStream) -> TokenStream,
        keep_going: impl FnOnce() -> TokenStream,
    ) -> TokenStream {
        self.zip_build_next_frame(cxt, is_own, frame_expr_pair_do, keep_going, true)
    }

    pub fn zip_build_next_frame(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        frame_expr_pair_do: impl FnOnce(TokenStream) -> TokenStream,
        keep_going: impl FnOnce() -> TokenStream,
        with_init: bool,
    ) -> TokenStream {
        let id = self.param_id();
        match self.data() {
            DataTyp::Leaf(_) => keep_going(),

            DataTyp::One(one) if one.is_self_rec() => one.extract_expr(
                id,
                is_own,
                |inner| {
                    let build_frame = cxt[self.e_idx]
                        .frames()
                        .expect("trying to build a frame for an expression type with no frames")
                        .to_build_tokens(self.v_idx, self.d_idx, is_own)
                        .expect("trying to build a frame for some expression data with no frames");
                    frame_expr_pair_do(quote! {
                        (#build_frame, #inner)
                    })
                },
                // keep_going,
            ),

            DataTyp::One(one) => {
                debug_assert!(!one.is_self_rec());

                let zip_fun = &cxt[one.inner()].self_ids().zip_fun;
                let keep_going = keep_going();

                let tokens = one.extract_expr(
                    id,
                    is_own,
                    |inner| {
                        quote! {
                            self.#zip_fun(#inner)
                        }
                    },
                    // || quote!(#id),
                );

                quote! {
                    let #id = #tokens;
                    #keep_going
                }
            }

            DataTyp::Many(many) => {
                let init = if with_init {
                    let initializer =
                        &cxt[many.e_idx()].coll_handlers()[many.c_idx()].initializer();
                    let acc = {
                        let init = initializer.to_call_tokens();
                        let step_field = &cxt.zip_ids().self_step_field();
                        cxt.lib_gen()
                            .zip_do_early_return_if_not_down(quote! { #step_field.#init })
                    };
                    let iter = {
                        let iter_fun = many.iter_fun(is_own);
                        quote! { #id.#iter_fun() }
                    };
                    let coll_der = cxt.lib_gen().coll_der_new(acc, iter);
                    quote!(let mut #id = #coll_der;)
                } else {
                    quote!()
                };

                let next_id = rust::Id::new("fast_expr_reserved_next", gen::span());
                let acc_field = cxt.lib_gen().coll_der_acc_field();
                let iter_field = cxt.lib_gen().coll_der_iter_field();

                if many.is_self_rec() {
                    let keep_going = keep_going();
                    let build_frame = cxt[self.e_idx]
                        .frames()
                        .expect("trying to build a frame for an expression type with no frames")
                        .to_build_tokens(self.v_idx, self.d_idx, is_own)
                        .expect("trying to build a frame for some expression data with no frames");
                    let and_then = frame_expr_pair_do(quote! {
                        (#build_frame, #next_id)
                    });

                    quote!(
                        #init
                        if let Some(#next_id) = #id.#iter_field.next() {
                            #and_then
                        } else {
                            let #id = #id.#acc_field;
                            #keep_going
                        }
                    )
                } else {
                    let zip_fun = &cxt[many.inner()].self_ids().zip_fun;
                    let fold_to_down = {
                        let folder = &cxt[many.e_idx()].coll_handlers()[many.c_idx()].folder();
                        let fold = folder.to_call_tokens(cxt, &next_id);
                        let step_field = &cxt.zip_ids().self_step_field();
                        cxt.lib_gen()
                            .zip_do_early_return_if_not_down(quote!(#step_field.#fold))
                    };
                    let keep_going = keep_going();
                    quote!(
                        #init
                        while let Some(#next_id) = #id.#iter_field.next() {
                            let #next_id = self.#zip_fun(#next_id);
                            #id.#acc_field = #fold_to_down;
                        }
                        let #id = #id.#acc_field;
                        #keep_going
                    )
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataTyp {
    Leaf(Leaf),
    One(One),
    Many(Many),
}
impl DataTyp {
    pub fn map_rec_exprs(&self, mut action: impl FnMut(idx::Expr, IsColl) -> Res<()>) -> Res<()> {
        match self {
            Self::Leaf(leaf) => leaf.map_rec_exprs(|idx| action(idx, false)),
            Self::One(one) => one.map_rec_exprs(|idx| action(idx, false)),
            Self::Many(many) => many.map_rec_exprs(|idx| action(idx, true)),
        }
    }

    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Leaf(_) => true,
            Self::One(_) | Self::Many(_) => false,
        }
    }
}

impl DataTyp {
    pub fn typ(&self) -> &rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.typ(),
            Self::One(one) => one.typ(),
            Self::Many(many) => many.typ(),
        }
    }

    pub fn is_self_rec(&self) -> bool {
        match self {
            Self::Leaf(_) => false,
            Self::One(one) => one.is_self_rec(),
            Self::Many(many) => many.is_self_rec(),
        }
    }

    pub fn needs_frame(&self) -> bool {
        match self {
            Self::Leaf(leaf) => leaf.needs_frame(),
            Self::One(one) => one.needs_frame(),
            Self::Many(many) => many.needs_frame(),
        }
    }

    pub fn frame_typ(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.frame_typ(cxt, is_own),
            Self::One(one) => one.frame_typ(cxt, is_own),
            Self::Many(many) => many.frame_typ(cxt, is_own),
        }
    }
    pub fn frame_der(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> Option<rust::Typ> {
        match self {
            Self::Leaf(leaf) => leaf.frame_der(cxt, is_own),
            Self::One(one) => one.frame_der(cxt, is_own),
            Self::Many(many) => many.frame_der(cxt, is_own),
        }
    }
    pub fn frame_res(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.frame_res(cxt, is_own),
            Self::One(one) => one.frame_res(cxt, is_own),
            Self::Many(many) => many.frame_res(cxt, is_own),
        }
    }
    pub fn zip_res(&self, cxt: &impl cxt::PreCxtLike, is_own: IsOwn) -> rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.zip_res(cxt, is_own),
            Self::One(one) => one.zip_res(cxt, is_own),
            Self::Many(many) => many.zip_res(cxt, is_own),
        }
    }

    pub fn inner(&self) -> Option<idx::Expr> {
        match self {
            Self::Leaf(_) => None,
            Self::One(one) => Some(one.inner()),
            Self::Many(many) => Some(many.inner()),
        }
    }
}

impl DataTyp {
    pub fn to_expr_data_tokens(&self, stream: &mut TokenStream) {
        match self {
            Self::Leaf(leaf) => leaf.typ().to_tokens(stream),
            Self::One(one) => one.typ().to_tokens(stream),
            Self::Many(many) => many.typ().to_tokens(stream),
        }
    }
}

implement! {
    impl Data {
        Display {
            |self, fmt| write!(
                fmt,
                "{}{}",
                self.d_id().map(|id| format!("{}: ", id)).unwrap_or_else(String::new),
                self.typ().to_token_stream()
            )
        }
        Deref<DataTyp> {
            |self| &self.data
        }
    }

    impl DataTyp {
        From<Leaf> {
            |leaf| Self::Leaf(leaf)
        }
        From<One> {
            |one| Self::One(one)
        }
        From<Many> {
            |many| Self::Many(many)
        }
    }
}

pub mod front {
    use super::*;

    /// True if the type mentions `Self` or an path-prefix-free expression id appearing in `cxt`.
    pub fn mentions_expr(cxt: &cxt::PreCxt, typ: &rust::Typ) -> Res<bool> {
        let mut todo: Vec<Either<&rust::Typ, &syn::Path>> = vec![Either::Left(typ)];

        while let Some(typ_or_path) = todo.pop() {
            use rust::Typ::*;
            match typ_or_path {
                Either::Right(path) => {
                    let mut path = path.segments.iter();
                    match (path.next(), path.next()) {
                        (Some(seg), None) => {
                            if seg.ident == "Self" || cxt.get_e_cxt(&seg.ident).is_some() {
                                return Ok(true);
                            }
                        }
                        _ => (),
                    }
                }

                Either::Left(Path(path)) => todo.push(Either::Right(&path.path)),

                Either::Left(Array(array)) => todo.push(Either::Left(&*array.elem)),
                Either::Left(Group(group)) => todo.push(Either::Left(&*group.elem)),
                Either::Left(Paren(typ)) => todo.push(Either::Left(&*typ.elem)),
                Either::Left(Reference(t_ref)) => todo.push(Either::Left(&*t_ref.elem)),
                Either::Left(Slice(slice)) => todo.push(Either::Left(&*slice.elem)),
                Either::Left(Tuple(tuple)) => {
                    for typ in &tuple.elems {
                        todo.push(Either::Left(typ))
                    }
                }
                Either::Left(TraitObject(trait_obj)) => {
                    for bound in &trait_obj.bounds {
                        match bound {
                            syn::TypeParamBound::Trait(bound) => {
                                todo.push(Either::Right(&bound.path))
                            }
                            syn::TypeParamBound::Lifetime(_) => (),
                        }
                    }
                }

                Either::Left(ImplTrait(impl_trait)) => {
                    bail!(on(impl_trait, "illegal impl trait in expression data"))
                }
                Either::Left(Ptr(ptr)) => bail!(on(ptr, "illegal raw pointer in expression data")),
                Either::Left(BareFn(fun)) => {
                    bail!(on(fun, "illegal bare function as expression data"))
                }
                Either::Left(Infer(infer)) => bail!(on(
                    infer,
                    "illegal `_` inference type, 
                    can only be used as argument for an expression type"
                )),
                Either::Left(Macro(mac)) => bail!(on(mac, "illegal macro in type position")),
                Either::Left(Never(never)) => bail!(on(never, "illegal `!` never type")),
                Either::Left(typ) => bail!(on(typ, "unexpected tokens")),
            }
        }

        Ok(false)
    }

    pub fn is_infer<'a>(mut args: impl Iterator<Item = &'a rust::GenericArg>) -> bool {
        match (args.next(), args.next()) {
            (Some(rust::GenericArg::Type(rust::Typ::Infer(_))), None) => true,
            _ => false,
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Rec {
        Slf(rust::Id),
        Expr {
            e_idx: idx::Expr,
            args: rust::GenericArgs,
        },
    }
    impl Rec {
        pub fn into_one(
            self,
            cxt: &mut cxt::PreCxt,
            e_idx: idx::Expr,
            v_idx: idx::Variant,
            d_idx: idx::Data,
            wrap: one::Wrap,
        ) -> One {
            match self {
                Rec::Slf(slf) => One::new_self(cxt, e_idx, v_idx, d_idx, slf, wrap),
                Rec::Expr { e_idx: inner, args } => {
                    One::new(cxt, e_idx, v_idx, d_idx, inner, args, wrap)
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum Resolved {
        None,
        Plain { wrap: one::Wrap, rec: Rec },
        Coll { coll: rust::Id, rec: Rec },
    }
    impl Resolved {
        pub fn into_data_typ(
            self,
            cxt: &mut cxt::PreCxt,
            e_idx: idx::Expr,
            v_idx: idx::Variant,
            d_idx: idx::Data,
            d_id: Option<&rust::Id>,
            typ: &rust::Typ,
        ) -> Res<DataTyp> {
            let res: DataTyp = match self {
                Self::None => Leaf::new(typ.clone()).into(),
                Self::Plain { wrap, rec } => {
                    let one = rec.into_one(cxt, e_idx, v_idx, d_idx, wrap);
                    cxt.register_expr_data(one.e_idx(), one.inner());
                    one.into()
                }
                Self::Coll { coll, rec } => {
                    let coll_span = coll.span();
                    let coll = many::Coll::from_id(&coll)?;
                    let one = rec.into_one(cxt, e_idx, v_idx, d_idx, one::Wrap::Plain);
                    let c_idx = cxt.register_expr_coll_data(e_idx, v_idx, d_idx, d_id, one.inner());
                    Many::new(cxt, coll_span, coll, one, c_idx)?.into()
                }
            };
            Ok(res)
        }

        pub fn is_none(&self) -> bool {
            if let Self::None = self {
                true
            } else {
                false
            }
        }
    }

    pub fn resolve_path(cxt: &mut cxt::PreCxt, path: &rust::Path) -> Res<Resolved> {
        let mut segments = path.segments.iter();

        let res = match segments.next() {
            Some(segment) if segment.ident == "Self" => {
                if !segment.arguments.is_empty() {
                    bail!(on(&segment.arguments, "illegal arguments for `Self` type"))
                }
                Resolved::Plain {
                    wrap: one::Wrap::Plain,
                    rec: Rec::Slf(segment.ident.clone()),
                }
            }

            Some(segment) if segment.ident == many::Coll::PREF => {
                if !segment.arguments.is_empty() {
                    bail!(on(
                        &segment.arguments,
                        "`{}` path segments does not take arguments",
                        many::Coll::PREF
                    ))
                }

                let id_segment = segments.next().ok_or_else(|| {
                    error!(on(
                        &segment.ident,
                        "expected collection type after this `{}` path segment",
                        many::Coll::PREF
                    ))
                })?;

                let coll = id_segment.ident.clone();
                let rec = {
                    let mut args = {
                        use syn::PathArguments::*;

                        match &id_segment.arguments {
                            AngleBracketed(args) => args.args.iter(),
                            None => bail!(on(
                                &id_segment.arguments,
                                "collections are only allowed to take a single expression type"
                            )),
                            Parenthesized(paren) => {
                                bail!(on(paren, "unexpected parenthesized arguments"))
                            }
                        }
                    };
                    match (args.next(), args.next()) {
                        (Some(rust::GenericArg::Type(typ)), None) => match resolve_typ(cxt, typ)? {
                            Resolved::Plain { wrap, rec } => {
                                if !wrap.is_plain() {
                                    bail!(on(typ, "illegal nesting of expression type wrappers"))
                                }
                                rec
                            }
                            _ => bail!(on(
                                &id_segment.arguments,
                                "collections are only allowed to take a single expression type"
                            )),
                        },
                        _ => bail!(on(
                            &id_segment.arguments,
                            "collections only take a single argument"
                        )),
                    }
                };

                Resolved::Coll { coll, rec }
            }

            Some(segment) if segment.ident == one::Wrap::PREF => {
                if !segment.arguments.is_empty() {
                    bail!(on(
                        &segment.arguments,
                        "`{}` path segments does not take arguments",
                        one::Wrap::PREF
                    ))
                }

                let id_segment = segments.next().ok_or_else(|| {
                    error!(on(
                        &segment.ident,
                        "expected wrapper type after this `{}` path segment",
                        one::Wrap::PREF
                    ))
                })?;

                let wrap = one::Wrap::from_id(&id_segment.ident)?;
                let rec = {
                    let mut args = {
                        use syn::PathArguments::*;

                        match &id_segment.arguments {
                            AngleBracketed(args) => args.args.iter(),
                            None => bail!(on(
                                &id_segment.arguments,
                                "wrappers are only allowed to take a single expression type"
                            )),
                            Parenthesized(paren) => {
                                bail!(on(paren, "unexpected parenthesized arguments"))
                            }
                        }
                    };
                    match (args.next(), args.next()) {
                        (Some(rust::GenericArg::Type(typ)), None) => match resolve_typ(cxt, typ)? {
                            Resolved::Plain { wrap, rec } => {
                                if !wrap.is_plain() {
                                    bail!(on(typ, "illegal nesting of expression type wrappers"))
                                }
                                rec
                            }
                            _ => bail!(on(
                                &id_segment.arguments,
                                "wrappers are only allowed to take a single expression type"
                            )),
                        },
                        _ => bail!(on(
                            &id_segment.arguments,
                            "wrappers only take a single argument"
                        )),
                    }
                };

                Resolved::Plain { wrap, rec }
            }

            Some(segment) => {
                if let Some(e_cxt) = cxt.get_e_cxt(&segment.ident) {
                    use syn::PathArguments::*;

                    let e_idx = e_cxt.e_idx();

                    match &segment.arguments {
                        None if e_cxt.generics().params.is_empty() => Resolved::Plain {
                            wrap: one::Wrap::Plain,
                            rec: Rec::Expr {
                                e_idx,
                                args: vec![],
                            },
                        },
                        None => bail!(on(
                            segment,
                            "this expression type takes type parameters, \
                            expected inference parameter `_`"
                        )),

                        AngleBracketed(args) => {
                            let args = if is_infer(args.args.iter()) {
                                e_cxt.top_t_params().clone()
                            } else if args.args.is_empty() && e_cxt.generics().params.is_empty() {
                                vec![]
                            } else {
                                let empty_msg = if e_cxt.generics().params.is_empty() {
                                    " or nothing"
                                } else {
                                    ""
                                };
                                bail!(on(segment, "expected inference parameter `_`{}", empty_msg))
                            };
                            Resolved::Plain {
                                wrap: one::Wrap::Plain,
                                rec: Rec::Expr { e_idx, args },
                            }
                        }

                        Parenthesized(paren) => {
                            bail!(on(paren, "unexpected parenthesized arguments"))
                        }
                    }
                } else {
                    Resolved::None
                }
            }
            None => bail!(on(path, "unexpected empty path")),
        };

        Ok(res)
    }

    pub fn resolve_typ(cxt: &mut cxt::PreCxt, typ: &rust::Typ) -> Res<Resolved> {
        let mut res = Resolved::None;

        {
            let mut typ = typ;
            'peel: loop {
                use rust::Typ::*;

                match typ {
                    Paren(paren) => typ = &*paren.elem,
                    Group(group) => typ = &*group.elem,
                    Path(path) => {
                        res = resolve_path(cxt, &path.path)?;
                        break 'peel;
                    }
                    _ => break 'peel,
                }
            }
        }

        if res.is_none() {
            if mentions_expr(cxt, typ)? {
                bail!(on(typ, "illegal recursive type"))
            }
        }

        Ok(res)
    }
}
