//! Generation of identifiers, types...

prelude! {}

pub fn span() -> rust::Span {
    rust::Span::mixed_site()
}

const FAST_EXPR_CRATE: &str = "fast_expr";

pub fn lib_path() -> Ident {
    Ident::new(FAST_EXPR_CRATE, gen::span())
}

pub mod fun {
    use super::*;

    pub const GO_UP_PREF: &str = "go_up";
    pub fn go_up(e_id: &Ident, v_id: &Ident) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("{}_{}_{}", GO_UP_PREF, e_id, v_id);
        Ident::new(&id, span())
    }

    const EVAL_SC_PREF: &str = "eval_sc";
    pub fn eval_sc(e_id: &Ident, v_id: &Ident) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("{}_{}_{}", EVAL_SC_PREF, e_id, v_id);
        Ident::new(&id, span())
    }
    pub fn variant_handler(e_id: &Ident, v_id: &Ident) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("handle_variant_{}_{}", e_id, v_id);
        Ident::new(&id, span())
    }

    pub fn initializer(e_id: &Ident, v_id: &Ident, d_id: &Ident) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("coll_init_{}_{}_{}", e_id, v_id, d_id);
        Ident::new(&id, span())
    }
    pub fn folder(e_id: &Ident, v_id: &Ident, d_id: &Ident) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let v_id = rust::try_snake_from(v_id);
        let id = format!("coll_fold_{}_{}_{}", e_id, v_id, d_id);
        Ident::new(&id, span())
    }

    pub mod param {
        use super::*;

        pub fn data_param(id: Either<&Ident, impl Display>) -> Ident {
            let id = match id.map_left(|id| {
                (
                    id,
                    rust::CamelIdent::try_from(id.clone()).and_then(|id| id.to_snake()),
                )
            }) {
                Either::Left((_, Ok(id))) => id.to_string(),
                Either::Left((id, Err(_))) => id.to_string(),
                Either::Right(idx) => format!("elem_{}", idx),
            };
            Ident::new(&id, span())
        }
    }
}

pub mod frame {
    use super::*;

    const SUFF: &str = "Frame";

    pub fn typ_id(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, SUFF), span())
    }
    pub fn variant_id(e_variant: &Ident, d_idx: idx::Data, field: Option<&Ident>) -> Ident {
        let id = match field
            .map(|id| rust::SnakeIdent::try_from(id.clone()).and_then(|id| id.to_camel()))
        {
            Some(Ok(id)) => format!("{}{}", e_variant, id),
            None | Some(Err(_)) => format!("{}{}", e_variant, d_idx),
        };
        Ident::new(&id, span())
    }

    pub fn sink_variant_id() -> Ident {
        Ident::new("Sink", span())
    }
}

pub mod lifetime {
    use super::*;

    pub fn expr() -> rust::Lifetime {
        rust::Lifetime {
            apostrophe: span(),
            ident: Ident::new("fast_expr", span()),
        }
    }
}

pub mod typ {
    use super::*;

    pub const RES_SUFF: &str = "Res";
    pub const ACC_SUFF: &str = "Acc";
    pub const ZIP_SUFF: &str = "Zip";
    pub const EVAL_SC_SUFF: &str = "EvalSc";

    pub fn res(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, RES_SUFF), span())
    }
    pub fn eval_sc(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, EVAL_SC_SUFF), span())
    }
    pub fn acc(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, ACC_SUFF), span())
    }
    pub fn zip(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, ZIP_SUFF), span())
    }

    pub mod param {
        use super::*;

        pub fn step() -> Ident {
            Ident::new("ZipSpec", span())
        }
        pub fn zip() -> Ident {
            Ident::new("Zip", span())
        }
    }
}

pub mod trai {
    use super::*;

    pub const ZIPPER_SUFF: &str = "ZipSpec";

    pub fn stepper(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, "Stepper"), span())
    }
    pub fn zipper(id: impl Display) -> Ident {
        Ident::new(&format!("{}{}", id, ZIPPER_SUFF), span())
    }

    pub mod lib {
        use super::*;

        pub fn stepper() -> Ident {
            Ident::new("Stepper", span())
        }
        pub fn zipper() -> Ident {
            Ident::new("Zipper", span())
        }
        pub fn zipper_res_typ() -> Ident {
            Ident::new("Res", span())
        }
    }
}

pub mod macr {
    use super::*;

    pub fn zipper_impl(e_id: &Ident, is_own: IsOwn) -> Ident {
        let e_id = rust::try_snake_from(e_id);
        let suff = rust::try_snake_from(&Ident::new(trai::ZIPPER_SUFF, span()));
        let own = if is_own { "own" } else { "ref" };
        Ident::new(&format!("impl_{}_{}_{}", e_id, suff, own), span())
    }
}

pub mod field {
    use super::*;

    pub mod expr_zip {
        use super::*;

        pub fn stepper() -> Ident {
            Ident::new("step", span())
        }
        pub fn stack(id: Ident) -> Ident {
            let id = match rust::CamelIdent::try_from(id.clone()).and_then(|id| id.to_snake()) {
                Ok(id) => format!("{}_stack", id),
                Err(_) => format!("{}_stack", id),
            };
            Ident::new(&id, span())
        }
        pub fn sink() -> Ident {
            Ident::new("_sink", span())
        }
    }
}

pub mod module {
    use super::*;

    pub fn zip_ref() -> Ident {
        Ident::new("zip_ref", span())
    }
    pub fn zip_own() -> Ident {
        Ident::new("zip_own", span())
    }
    pub fn zip(is_own: IsOwn) -> Ident {
        if is_own {
            zip_own()
        } else {
            zip_ref()
        }
    }
}

pub mod punct {
    // use super::*;

    use syn::punctuated::Punctuated;

    pub fn do_with<Elm, Punct>(
        punct: &Punctuated<Elm, Punct>,
        mut action: impl FnMut(Option<&Punct>),
    ) {
        let mut punct = punct.pairs();
        'puncts: loop {
            use syn::punctuated::Pair::*;
            let punct = match punct.next() {
                Some(Punctuated(_, punct)) => Some(punct),
                Some(End(_)) => {
                    debug_assert!(punct.next().is_none());
                    None
                }
                None => break 'puncts,
            };
            action(punct)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lib {
    path: Ident,
    internal_path: Ident,
}
impl Lib {
    pub fn new(conf: &conf::Conf) -> Self {
        let path = conf.fast_expr_name.deref().clone();
        let internal_path = Ident::new("internal", span());
        Self {
            path,
            internal_path,
        }
    }

    fn sink_id() -> Ident {
        Ident::new("Sink", span())
    }
    pub fn sink_instantiate(&self, tt: impl ToTokens) -> TokenStream {
        let path = &self.path;
        let internal = &self.internal_path;
        let sink = Self::sink_id();
        quote! { #path :: #internal :: #sink < #tt > }
    }
    pub fn sink_match_empty(&self, id: &Ident) -> TokenStream {
        quote! { (_, #id) }
    }

    fn empty_id() -> Ident {
        Ident::new("Empty", span())
    }
    pub fn empty_instantiate(&self) -> TokenStream {
        let path = &self.path;
        let internal = &self.internal_path;
        let empty = Self::empty_id();
        quote! { #path :: #internal :: #empty }
    }

    fn coll_der_id() -> Ident {
        Ident::new("CollDer", span())
    }
    pub fn coll_der_instantiate(&self, acc: impl ToTokens, iter: impl ToTokens) -> TokenStream {
        let path = &self.path;
        let coll_der = Self::coll_der_id();
        quote! {
            #path :: #coll_der < #acc, #iter >
        }
    }
    pub fn coll_der_acc_field(&self) -> Ident {
        Ident::new("acc", span())
    }
    pub fn coll_der_iter_field(&self) -> Ident {
        Ident::new("iter", span())
    }
    pub fn coll_der_new(&self, acc: impl ToTokens, iter: impl ToTokens) -> TokenStream {
        let path = &self.path;
        let coll_der = Self::coll_der_id();
        quote! {
            #path :: #coll_der :: new ( #acc, #iter )
        }
    }

    fn zip_do_id() -> Ident {
        Ident::new("ZipDo", span())
    }
    fn zip_do_go_down_id() -> Ident {
        Ident::new("GoDown", span())
    }
    fn zip_do_go_up_id() -> Ident {
        Ident::new("GoUp", span())
    }
    fn zip_do_subst_id() -> Ident {
        Ident::new("Subst", span())
    }
    fn zip_do_early_id() -> Ident {
        Ident::new("Early", span())
    }
    pub fn zip_do_instantiate(
        &self,
        down: impl ToTokens,
        expr: impl ToTokens,
        res: impl ToTokens,
    ) -> TokenStream {
        let path = &self.path;
        let zip_do = Self::zip_do_id();
        quote! {
            #path :: #zip_do < #down, #expr, #res >
        }
    }

    pub fn zip_do_match_cases(
        &self,
        down_pat: impl ToTokens,
        down_do: impl ToTokens,
        up_pat: impl ToTokens,
        up_do: impl ToTokens,
        subst_pat: impl ToTokens,
        subst_do: impl ToTokens,
        early_pat: impl ToTokens,
        early_do: impl ToTokens,
    ) -> TokenStream {
        let path = &self.path;
        let zip_do = Self::zip_do_id();
        let (down, up, subst, early) = (
            Self::zip_do_go_down_id(),
            Self::zip_do_go_up_id(),
            Self::zip_do_subst_id(),
            Self::zip_do_early_id(),
        );
        quote! {
            #path :: #zip_do :: #down (#down_pat) => #down_do,
            #path :: #zip_do :: #up (#up_pat) => #up_do,
            #path :: #zip_do :: #subst (#subst_pat) => #subst_do,
            #path :: #zip_do :: #early (#early_pat) => #early_do,
        }
    }

    pub fn zip_do_map_go_down(&self) -> Ident {
        Ident::new("map_go_down", span())
    }
    pub fn zip_do_go_down_and_then(&self) -> Ident {
        Ident::new("go_down_and_then", span())
    }
    pub fn zip_do_empty_convert(&self) -> Ident {
        Ident::new("empty_convert", span())
    }

    fn zip_do_new(&self, variant: Ident, inner: impl ToTokens) -> TokenStream {
        let lib_path = &self.path;
        let zip_do = Self::zip_do_id();
        quote! {
            #lib_path :: #zip_do :: #variant ( #inner )
        }
    }

    pub fn zip_do_new_go_down(&self, inner: impl ToTokens) -> TokenStream {
        self.zip_do_new(Self::zip_do_go_down_id(), inner)
    }
    pub fn zip_do_new_go_up(&self, inner: impl ToTokens) -> TokenStream {
        self.zip_do_new(Self::zip_do_go_up_id(), inner)
    }
    pub fn zip_do_new_subst(&self, inner: impl ToTokens) -> TokenStream {
        self.zip_do_new(Self::zip_do_subst_id(), inner)
    }
    pub fn zip_do_new_early(&self, inner: impl ToTokens) -> TokenStream {
        self.zip_do_new(Self::zip_do_early_id(), inner)
    }

    pub fn zip_do_early_return_if_not_go_down(&self, expr: impl ToTokens) -> TokenStream {
        let path = &self.path;
        let id = Self::zip_do_id();
        let go_down = Self::zip_do_go_down_id();
        let go_up = Self::zip_do_go_up_id();
        let subst = Self::zip_do_subst_id();
        let early = Self::zip_do_early_id();
        quote! {
            match #expr {
                #path :: #id :: #go_down (stuff) => stuff,
                #path :: #id :: #go_up (stuff) => return #path :: #id :: #go_up (stuff),
                #path :: #id :: #subst (stuff) => return #path :: #id :: #subst (stuff),
                #path :: #id :: #early (stuff) => return #path :: #id :: #early (stuff),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ZipBoundIds {
    pub zip_fun: Ident,
    pub inspect_fun: Ident,
    pub handle_frame_fun: Ident,
    pub handle_expr_fun: Ident,

    pub stack_field: Option<Ident>,
}
impl ZipBoundIds {
    pub fn new(expr: &expr::Expr) -> Self {
        let e_id = expr.e_id();

        let zip_fun = Self::gen(format!("zip_{}", rust::try_snake_from(e_id)));
        let inspect_fun = Self::gen(format!("inspect_{}", rust::try_snake_from(e_id)));
        let handle_frame_fun = Self::gen(format!("handle_frame_{}", rust::try_snake_from(e_id)));
        let handle_expr_fun = Self::gen(format!("handle_{}", rust::try_snake_from(e_id)));

        let stack_field = if expr.is_self_rec() {
            Some(Self::gen(format!("stack_{}", rust::try_snake_from(e_id))))
        } else {
            None
        };

        Self {
            zip_fun,
            inspect_fun,
            handle_frame_fun,
            handle_expr_fun,

            stack_field,
        }
    }

    fn gen(s: impl AsRef<str>) -> Ident {
        Ident::new(s.as_ref(), span())
    }
}

#[derive(Debug, Clone)]
pub struct ZipIds {
    pub expr_var: Ident,
    pub new_expr_var: Ident,
    pub res_var: Ident,
    pub new_res_var: Ident,
    pub frame_var: Ident,
    pub depth_var: Ident,
    pub zip_do_var: Ident,

    pub step_field: Ident,
}
impl ZipIds {
    fn gen_reserved(name: impl Display) -> Ident {
        Ident::new(&format!("{}_reserved_for_fast_expr", name), span())
    }
    fn gen(s: impl AsRef<str>) -> Ident {
        Ident::new(s.as_ref(), span())
    }

    pub fn self_step_field(&self) -> TokenStream {
        let step_field = &self.step_field;
        quote!(self.#step_field)
    }

    pub fn zip_fun(e_id: &Ident) -> Ident {
        Self::gen(format!("zip_{}", rust::try_snake_from(e_id)))
    }
    pub fn inspect_fun(e_id: &Ident) -> Ident {
        Self::gen(format!("inspect_{}", rust::try_snake_from(e_id)))
    }
    pub fn handle_frame_fun(e_id: &Ident) -> Ident {
        Self::gen(format!("handle_frame_{}", rust::try_snake_from(e_id)))
    }
    pub fn handle_expr_fun(e_id: &Ident) -> Ident {
        Self::gen(format!("handle_{}", rust::try_snake_from(e_id)))
    }

    pub fn stack_field(e_id: &Ident) -> Ident {
        Self::gen(format!("stack_{}", rust::try_snake_from(e_id)))
    }
}
impl Default for ZipIds {
    fn default() -> Self {
        Self {
            expr_var: Self::gen_reserved("expr"),
            new_expr_var: Self::gen_reserved("new_expr"),
            res_var: Self::gen_reserved("res"),
            new_res_var: Self::gen_reserved("new_res"),
            frame_var: Self::gen_reserved("frame"),
            depth_var: Self::gen_reserved("depth"),
            zip_do_var: Self::gen_reserved("zip_do"),

            step_field: Self::gen_reserved("step"),
        }
    }
}
