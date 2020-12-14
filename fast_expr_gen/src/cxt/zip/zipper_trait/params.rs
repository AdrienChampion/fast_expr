//! Function parameters for variant/collection handlers.

prelude! {}

#[derive(Debug, Clone, Copy)]
pub struct Data {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,
}
implement! {
    impl Data {
        From<(idx::Expr, idx::Variant, idx::Data)> {
            |(e_idx, v_idx, d_idx)| Self {
                e_idx, v_idx, d_idx
            }
        }
    }
    impl('a) Data {
        From<&'a expr::Data> {
            |data| Self {
                e_idx: data.e_idx(),
                v_idx: data.v_idx(),
                d_idx: data.d_idx(),
            }
        }
    }
}

/// Indicates where a parameter comes from.
///
/// Used in variant/collection handlers. The goal is to make it easy for helper traits/macros to go
/// over the parameters and know which ones they need to change for what they encode.
#[derive(Debug, Clone)]
pub enum ParamKind {
    Leaf { is_ref: bool },
    One { is_ref: bool, is_res: bool },
    Many { is_ref: bool, is_res: bool },
    ManyIter { acc: Type, res: Type },
}
impl ParamKind {
    pub fn res_from(data: &expr::Data, is_ref: bool) -> Self {
        match data.data() {
            expr::data::DataType::Leaf(_) => Self::Leaf { is_ref },
            expr::data::DataType::One(_) => Self::One {
                is_ref,
                is_res: true,
            },
            expr::data::DataType::Many(_) => Self::Many {
                is_ref,
                is_res: true,
            },
        }
    }
    pub fn res_ref_from(data: &expr::Data) -> Self {
        Self::res_from(data, true)
    }
    pub fn frame_from(data: &expr::Data, is_ref: bool) -> Self {
        match data.data() {
            expr::data::DataType::Leaf(_) => Self::Leaf { is_ref },
            expr::data::DataType::One(_) => Self::One {
                is_ref,
                is_res: false,
            },
            expr::data::DataType::Many(_) => Self::Many {
                is_ref,
                is_res: false,
            },
        }
    }

    pub fn typ(
        &self,
        cxt: &impl cxt::FrameCxtLike,
        Data {
            e_idx,
            v_idx,
            d_idx,
        }: Data,
        is_own: IsOwn,
    ) -> Type {
        let data = &cxt.get_frame_e_cxt(e_idx).expr()[v_idx][d_idx];
        match *self {
            Self::Leaf { is_ref } => {
                rust::typ::reference_if(is_ref, None, data.frame_typ(cxt, is_own))
            }
            Self::One { is_ref, is_res } | Self::Many { is_ref, is_res } => {
                rust::typ::reference_if(
                    is_ref,
                    None,
                    if is_res {
                        data.zip_res(cxt, is_own)
                    } else {
                        data.frame_typ(cxt, is_own)
                    },
                )
            }
            Self::ManyIter { ref acc, ref res } => {
                syn::parse_quote! { (#acc, #res) }
            }
        }
    }

    pub fn typ_tokens(
        &self,
        cxt: &impl cxt::FrameCxtLike,
        Data {
            e_idx,
            v_idx,
            d_idx,
        }: Data,
        is_own: IsOwn,
        expr_lt: Option<&TokenStream>,
    ) -> TokenStream {
        let data = &cxt.get_frame_e_cxt(e_idx).expr()[v_idx][d_idx];
        match *self {
            Self::Leaf { is_ref } => {
                let mut typ = data.frame_typ_tokens(cxt, is_own, expr_lt);
                if is_ref {
                    typ = quote! { & #typ }
                }
                typ
            }
            Self::One { is_ref, is_res } | Self::Many { is_ref, is_res } => {
                let mut typ = if is_res {
                    data.zip_res(cxt, is_own).to_token_stream()
                } else {
                    data.frame_typ_tokens(cxt, is_own, expr_lt)
                };
                if is_ref {
                    typ = quote! { & #typ }
                }
                typ
            }
            Self::ManyIter { ref acc, ref res } => {
                quote! { (#acc, #res) }
            }
        }
    }

    pub fn typ_with_forced_many_acc(
        &self,
        cxt: &impl cxt::FrameCxtLike,
        Data {
            e_idx,
            v_idx,
            d_idx,
        }: Data,
        is_own: IsOwn,
        many_acc: impl FnOnce() -> Type,
    ) -> Type {
        match *self {
            Self::Many {
                is_ref,
                is_res: true,
            } => rust::typ::reference_if(is_ref, None, many_acc()),
            Self::ManyIter { ref acc, ref res } => {
                let _ = acc;
                let acc = many_acc();
                syn::parse_quote! { (#acc, #res) }
            }
            _ => self.typ(
                cxt,
                Data {
                    e_idx,
                    v_idx,
                    d_idx,
                },
                is_own,
            ),
        }
    }
}

pub type FnParams = idx::DataMap<FnParam>;

#[derive(Debug, Clone)]
pub struct FnParam {
    id: Ident,
    data: Data,
    kind: ParamKind,
}
impl FnParam {
    fn id_gen(data: &expr::Data) -> Ident {
        gen::fun::param::data_param(
            data.d_id()
                .map(Either::Left)
                .unwrap_or_else(|| Either::Right(data.d_idx())),
        )
    }

    pub fn res_from_data(data: &expr::Data) -> Self {
        let id = Self::id_gen(data);

        let kind = ParamKind::res_from(data, false);
        let data = data.into();

        Self { id, data, kind }
    }

    pub fn res_ref_from_data(id: Option<Ident>, data: &expr::Data) -> Self {
        let id = id.unwrap_or_else(|| Self::id_gen(data));

        let kind = ParamKind::res_from(data, true);
        let data = data.into();

        Self { id, data, kind }
    }

    pub fn typ_from_data(id: Option<Ident>, data: &expr::Data) -> Self {
        let id = id.unwrap_or_else(|| Self::id_gen(data));

        let kind = ParamKind::frame_from(data, true);
        let data = data.into();

        Self { id, data, kind }
    }
    pub fn typ_ref_from_data(id: Option<Ident>, data: &expr::Data) -> Self {
        let id = id.unwrap_or_else(|| Self::id_gen(data));

        let kind = ParamKind::frame_from(data, true);
        let data = data.into();

        Self { id, data, kind }
    }

    pub fn many_iter(id: Option<Ident>, data: &expr::Data, (acc, res): (Type, Type)) -> Self {
        let id = id.unwrap_or_else(|| Self::id_gen(data));

        let kind = ParamKind::ManyIter { acc, res };
        let data = data.into();

        Self { id, data, kind }
    }

    pub fn id(&self) -> &Ident {
        &self.id
    }
    pub fn typ(&self, cxt: &impl cxt::FrameCxtLike, is_own: IsOwn) -> Type {
        self.kind.typ(cxt, self.data, is_own)
    }
    pub fn typ_with_forced_many_acc(
        &self,
        cxt: &impl cxt::FrameCxtLike,
        is_own: IsOwn,
        many_acc: impl FnOnce() -> Type,
    ) -> Type {
        self.kind
            .typ_with_forced_many_acc(cxt, self.data, is_own, many_acc)
    }

    pub fn to_tokens(
        &self,
        cxt: &impl cxt::FrameCxtLike,
        is_own: IsOwn,
        expr_lt: Option<&TokenStream>,
    ) -> TokenStream {
        let id = &self.id;
        let typ = self.kind.typ_tokens(cxt, self.data, is_own, expr_lt);
        quote!(#id: #typ)
    }
}
