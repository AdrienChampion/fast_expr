//! Variant data representation.

prelude! {}

use cxt::Cxt;

/// Some variant data.
#[derive(Debug, Clone)]
pub struct Data {
    data: DataTyp,

    src: rust::Field,
}

impl Data {
    pub fn from_front(
        cxt: &Cxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        field: &rust::Field,
    ) -> Res<Self> {
        let src = field.clone();
        let typ = &field.ty;

        let data_typ: DataTyp =
            self::front::resolve_typ(cxt, typ)?.into_data_typ(cxt, e_idx, v_idx, d_idx, typ)?;

        Ok(Self {
            data: data_typ.into(),
            src,
        })
    }

    pub fn id(&self) -> Option<&rust::Id> {
        self.src.ident.as_ref()
    }

    pub fn typ(&self) -> &rust::Typ {
        self.data.typ()
    }
}

impl Data {
    pub fn to_expr_data_tokens(&self, stream: &mut TokenStream) {
        logln!("- {}", self);
        stream.append_all(&self.src.attrs);
        self.src.vis.to_tokens(stream);
        if let Some(ident) = &self.src.ident {
            ident.to_tokens(stream);
            if let Some(token) = self.src.colon_token {
                token.to_tokens(stream)
            } else {
                syn::token::Colon {
                    spans: [rust::Span::mixed_site()],
                }
                .to_tokens(stream)
            }
        }
        self.data.to_expr_data_tokens(stream);
    }
}

#[derive(Debug, Clone)]
pub enum DataTyp {
    Leaf(Leaf),
    One(One),
    Many(Many),
}
impl DataTyp {
    pub fn typ(&self) -> &rust::Typ {
        match self {
            Self::Leaf(leaf) => leaf.typ(),
            Self::One(one) => one.typ(),
            Self::Many(many) => many.typ(),
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

/// A leaf is just some data of some type that's not a sub-expression type.
#[derive(Debug, Clone)]
pub struct Leaf {
    typ: rust::Typ,
}
impl Leaf {
    /// Constructor.
    pub fn new(typ: rust::Typ) -> Self {
        Self { typ }
    }

    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }
}

/// Represents a variant that stores one type of self-expression.
#[derive(Debug, Clone)]
pub struct One {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,
    inner: idx::Expr,
    id: rust::Id,
    args: Option<rust::GenericArgs>,
    typ: rust::Typ,
    wrap: Wrap,
}
impl One {
    /// Constructor.
    pub fn new_self(
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        slf: rust::Id,
        wrap: Wrap,
    ) -> Self {
        debug_assert_eq!(slf, "Self");
        let typ = rust::typ::plain(slf.clone(), None);
        Self {
            e_idx,
            v_idx,
            d_idx,
            inner: e_idx,
            id: slf,
            args: None,
            typ,
            wrap,
        }
    }

    /// Constructor.
    pub fn new(
        cxt: &Cxt,
        e_idx: idx::Expr,
        v_idx: idx::Variant,
        d_idx: idx::Data,
        inner: idx::Expr,
        args: rust::GenericArgs,
        wrap: Wrap,
    ) -> Self {
        let id = cxt[inner].id().clone();
        let args = Some(args);
        let typ = rust::typ::plain(id.clone(), args.clone());
        Self {
            e_idx,
            v_idx,
            d_idx,
            inner,
            id,
            args,
            typ,
            wrap,
        }
    }

    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }
}

#[derive(Debug, Clone)]
pub struct Many {
    coll_span: rust::Span,
    coll: Coll,
    inner: One,
    typ: rust::Typ,
}
impl Many {
    pub fn new(coll_span: rust::Span, coll: Coll, inner: One) -> Self {
        let (path, id) = coll.to_path();
        let path = path.iter().map(|id| rust::Id::new(id, coll_span));
        let id = rust::Id::new(id, coll_span);
        let args = vec![rust::GenericArg::Type(inner.typ().clone())];
        let typ = rust::typ::simple_path(path, id, Some(args));
        Self {
            coll_span,
            coll,
            inner,
            typ,
        }
    }

    pub fn typ(&self) -> &rust::Typ {
        &self.typ
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Coll {
    Vec,
    HashSet,
    BTreeSet,
}
impl Coll {
    pub const PREF: &'static str = "coll";
    const COLL_PATH: [&'static str; 2] = ["std", "collection"];
    const VEC_PATH: [&'static str; 2] = ["std", "vec"];
    const VEC: &'static str = "Vec";
    const HASH_SET: &'static str = "HashSet";
    const BTREE_SET: &'static str = "BTreeSet";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == Self::VEC {
            Ok(Self::Vec)
        } else if id == Self::HASH_SET {
            Ok(Self::HashSet)
        } else if id == Self::BTREE_SET {
            Ok(Self::BTreeSet)
        } else {
            bail!(on(id, "unknown collection type"))
        }
    }
    pub fn to_path(self) -> (&'static [&'static str], &'static str) {
        match self {
            Self::Vec => (&Self::VEC_PATH, Self::VEC),
            Self::HashSet => (&Self::COLL_PATH, Self::HASH_SET),
            Self::BTreeSet => (&Self::COLL_PATH, Self::BTREE_SET),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wrap {
    Plain,
    Box,
    Ref(rust::Lifetime),
}
impl Wrap {
    pub const PREF: &'static str = "wrap";
    const BOX_PREF: [&'static str; 2] = ["std", "boxed"];
    const BOX: &'static str = "Box";

    pub fn from_id(id: &rust::Id) -> Res<Self> {
        if id == Self::BOX {
            Ok(Wrap::Box)
        } else {
            bail!(on(id, "unknown wrapper type"))
        }
    }

    pub fn is_plain(&self) -> bool {
        *self == Self::Plain
    }
}

implement! {
    impl Data {
        Display {
            |self, fmt| write!(
                fmt,
                "{}{}",
                self.id().map(|id| format!("{}: ", id)).unwrap_or_else(String::new),
                self.typ().to_token_stream()
            )
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
    pub fn mentions_expr(cxt: &Cxt, typ: &rust::Typ) -> Res<bool> {
        let mut todo: Vec<Either<&rust::Typ, &syn::Path>> = vec![Either::Left(typ)];

        while let Some(typ_or_path) = todo.pop() {
            use rust::Typ::*;
            match typ_or_path {
                Either::Right(path) => {
                    let mut path = path.segments.iter();
                    match (path.next(), path.next()) {
                        (Some(seg), None) => {
                            if seg.ident == "Self" || cxt.get_expr(&seg.ident).is_some() {
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
            cxt: &Cxt,
            e_idx: idx::Expr,
            v_idx: idx::Variant,
            d_idx: idx::Data,
            wrap: Wrap,
        ) -> One {
            match self {
                Rec::Slf(slf) => One::new_self(e_idx, v_idx, d_idx, slf, wrap),
                Rec::Expr { e_idx: inner, args } => {
                    One::new(cxt, e_idx, v_idx, d_idx, inner, args, wrap)
                }
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Resolved {
        None,
        Plain { wrap: Wrap, rec: Rec },
        Coll { coll: rust::Id, rec: Rec },
    }
    impl Resolved {
        pub fn into_data_typ(
            self,
            cxt: &Cxt,
            e_idx: idx::Expr,
            v_idx: idx::Variant,
            d_idx: idx::Data,
            typ: &rust::Typ,
        ) -> Res<DataTyp> {
            let res: DataTyp = match self {
                Self::None => Leaf::new(typ.clone()).into(),
                Self::Plain { wrap, rec } => rec.into_one(cxt, e_idx, v_idx, d_idx, wrap).into(),
                Self::Coll { coll, rec } => {
                    let coll_span = coll.span();
                    let coll = Coll::from_id(&coll)?;
                    let inner = rec.into_one(cxt, e_idx, v_idx, d_idx, Wrap::Plain);
                    Many::new(coll_span, coll, inner).into()
                }
            };
            Ok(res)
        }
    }

    pub fn resolve_path(cxt: &Cxt, path: &rust::Path) -> Res<Resolved> {
        let mut segments = path.segments.iter();

        let res = match segments.next() {
            Some(segment) if segment.ident == "Self" => {
                if !segment.arguments.is_empty() {
                    bail!(on(&segment.arguments, "illegal arguments for `Self` type"))
                }
                Resolved::Plain {
                    wrap: Wrap::Plain,
                    rec: Rec::Slf(segment.ident.clone()),
                }
            }

            Some(segment) if segment.ident == Coll::PREF => {
                if !segment.arguments.is_empty() {
                    bail!(on(
                        &segment.arguments,
                        "`{}` path segments does not take arguments",
                        Coll::PREF
                    ))
                }

                let id_segment = segments.next().ok_or_else(|| {
                    error!(on(
                        &segment.ident,
                        "expected collection type after this `{}` path segment",
                        Coll::PREF
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

            Some(segment) if segment.ident == Wrap::PREF => {
                if !segment.arguments.is_empty() {
                    bail!(on(
                        &segment.arguments,
                        "`{}` path segments does not take arguments",
                        Wrap::PREF
                    ))
                }

                let id_segment = segments.next().ok_or_else(|| {
                    error!(on(
                        &segment.ident,
                        "expected wrapper type after this `{}` path segment",
                        Wrap::PREF
                    ))
                })?;

                let wrap = Wrap::from_id(&id_segment.ident)?;
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
                if let Some(e_cxt) = cxt.get_expr(&segment.ident) {
                    use syn::PathArguments::*;

                    let e_idx = e_cxt.e_idx();

                    match &segment.arguments {
                        None => Resolved::Plain {
                            wrap: Wrap::Plain,
                            rec: Rec::Expr {
                                e_idx,
                                args: vec![],
                            },
                        },
                        AngleBracketed(args) => {
                            let args = if is_infer(args.args.iter()) {
                                e_cxt.top_t_params().clone()
                            } else {
                                args.args.iter().cloned().collect()
                            };
                            Resolved::Plain {
                                wrap: Wrap::Plain,
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

    pub fn resolve_typ(cxt: &Cxt, typ: &rust::Typ) -> Res<Resolved> {
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

        if res == Resolved::None {
            if mentions_expr(cxt, typ)? {
                bail!(on(typ, "illegal recursive type"))
            }
        }

        Ok(res)
    }
}
