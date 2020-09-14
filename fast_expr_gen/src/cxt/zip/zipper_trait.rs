//! Handles data specific to the zipper trait and its implementations.

prelude! {}

#[derive(Debug, Clone)]
pub struct FnParam {
    id: rust::Id,
    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl FnParam {
    pub fn from_data(cxt: &cxt::FrameCxt, data: &expr::Data) -> Self {
        let id = gen::fun::param::data_param(
            data.d_id()
                .map(Either::Left)
                .unwrap_or_else(|| Either::Right(data.d_idx())),
        );
        let own_typ = data.zip_res(cxt, true);
        let ref_typ = data.zip_res(cxt, false);
        Self {
            id,
            own_typ,
            ref_typ,
        }
    }

    pub fn new(id: rust::Id, own_typ: rust::Typ, ref_typ: rust::Typ) -> Self {
        Self {
            id,
            own_typ,
            ref_typ,
        }
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }
    pub fn typ(&self, is_own: IsOwn) -> &rust::Typ {
        if is_own {
            &self.own_typ
        } else {
            &self.ref_typ
        }
    }

    pub fn to_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let typ = self.typ(is_own);
        quote!(#id: #typ)
    }
}

#[derive(Debug, Clone)]
pub struct Inspecter {
    e_idx: idx::Expr,

    id: rust::Id,

    assoc_res_typ: rust::Id,
}
impl Inspecter {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let id = cxt[e_idx].self_ids().inspect_fun.clone();
        let assoc_res_typ = cxt[e_idx].res_typ_id().clone();
        Self {
            e_idx,
            id,
            assoc_res_typ,
        }
    }

    pub fn fun_inspect_self_sig_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        expr_param: impl ToTokens,
        expr_typ: Option<TokenStream>,
    ) -> TokenStream {
        let e_cxt = &cxt[self.e_idx];
        let id = &self.id;
        let e_typ = expr_typ.unwrap_or_else(|| e_cxt.plain_typ_for(is_own).to_token_stream());
        let res_typ = {
            let res = e_cxt.res_typ_id();
            let res = quote!(Self::#res);
            cxt.lib_gen().zip_do_instantiate(&e_typ, &e_typ, &res)
        };
        quote! {
            fn #id (&mut self, #expr_param: #e_typ) -> #res_typ
        }
    }
    pub fn fun_inspect_self_def_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let expr_param = quote!(expr);
        let sig = self.fun_inspect_self_sig_tokens(cxt, is_own, &expr_param, None);
        let def = cxt.lib_gen().zip_do_new_go_down(expr_param);

        quote! {
            #sig {
                #def
            }
        }
    }

    pub fn to_zipper_trait_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        for_expr: idx::Expr,
        is_own: IsOwn,
    ) -> TokenStream {
        let expr_param = quote!(expr);
        let sig = self.fun_inspect_self_sig_tokens(cxt, is_own, &expr_param, None);
        let def = cxt.lib_gen().zip_do_new_go_down(expr_param);

        let assoc_res_doc = doc::zipper_trait::res_typ_doc(cxt, self.e_idx, for_expr);
        let assoc_res = &self.assoc_res_typ;

        quote! {
            #assoc_res_doc
            type #assoc_res;
            #sig {
                #def
            }
        }
    }
}

pub type VariantHandlers = idx::VariantMap<VariantHandler>;
impl VariantHandlers {
    pub fn from(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        cxt[e_idx]
            .expr()
            .variants()
            .iter()
            .map(|variant| VariantHandler::new(cxt, e_idx, variant))
            .collect()
    }

    pub fn to_zipper_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let handlers = self
            .iter()
            .map(|handler| handler.to_zipper_trait_tokens(cxt, is_own));
        quote! {
            #(#handlers)*
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariantHandler {
    e_idx: idx::Expr,
    v_idx: idx::Variant,

    go_up_id: rust::Id,
    go_up_params: idx::DataMap<FnParam>,
}
impl VariantHandler {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr, variant: &expr::Variant) -> Self {
        let v_idx = variant.v_idx();

        let go_up_id = variant.zipper_go_up_id().clone();
        let go_up_params = variant
            .data()
            .iter()
            .map(|data| FnParam::from_data(cxt, data))
            .collect();

        Self {
            e_idx,
            v_idx,

            go_up_id,
            go_up_params,
        }
    }

    pub fn to_go_up_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.go_up_id;
        let e_cxt = &cxt[self.e_idx];
        let params = self
            .go_up_params
            .iter()
            .map(|param| param.to_tokens(is_own));
        let go_up_res = {
            let down_typ = cxt.lib_gen().empty_instantiate();
            let expr_typ = e_cxt.plain_typ_for(is_own);
            let res_typ = e_cxt.res_typ_id();
            cxt.lib_gen()
                .zip_do_instantiate(quote!(#down_typ), expr_typ, quote!(Self::#res_typ))
        };
        let go_up_doc = doc::zipper_trait::go_up_doc(cxt, self.e_idx, self.v_idx);
        quote! {
            #go_up_doc
            fn #id (&mut self, #(#params,)* ) -> #go_up_res;
        }
    }

    pub fn to_zipper_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.to_go_up_tokens(cxt, is_own)
    }
}

#[derive(Debug, Clone)]
pub struct CollFolder {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,
    c_idx: idx::Coll,

    id: rust::Id,

    params: idx::DataMap<FnParam>,
    assoc_acc_typ: rust::Id,
}

impl CollFolder {
    pub fn new(cxt: &cxt::FrameCxt, coll: &cxt::CollCxt, assoc_acc_typ: &rust::Id) -> Self {
        let assoc_acc_typ = assoc_acc_typ.clone();
        let e_idx = coll.e_idx();
        let v_idx = coll.v_idx();
        let d_idx = coll.d_idx();
        let c_idx = coll.c_idx();

        let e_cxt = &cxt[e_idx];
        let expr = e_cxt.expr();

        let id = gen::fun::folder(
            e_cxt.e_id(),
            expr[v_idx].v_id(),
            expr[v_idx][d_idx].param_id(),
        );

        let params = expr[v_idx]
            .data()
            .index_iter()
            .map(|(p_d_idx, p_data)| {
                let id = p_data.param_id().clone();
                if p_d_idx < d_idx {
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.zip_res(cxt, true)),
                        rust::typ::reference(None, p_data.zip_res(cxt, false)),
                    )
                } else if p_d_idx == d_idx {
                    let inner = p_data
                        .inner()
                        .expect("trying to build a folder over non-recursive data");
                    let res = cxt[inner].res_typ_id();
                    let typ: rust::Typ = syn::parse_quote! {
                        (Self::#assoc_acc_typ, Self::#res)
                    };
                    FnParam::new(id, typ.clone(), typ)
                } else {
                    debug_assert!(p_d_idx > d_idx);
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.frame_typ(cxt, true)),
                        rust::typ::reference(None, p_data.frame_typ(cxt, false)),
                    )
                }
            })
            .collect();

        Self {
            e_idx,
            v_idx,
            d_idx,
            c_idx,

            id,

            params,

            assoc_acc_typ,
        }
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }

    pub fn to_decl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let params = self.params.iter().map(|param| param.to_tokens(is_own));
        let zip_do = {
            let acc_t_param = &self.assoc_acc_typ;
            let expr_typ = cxt[self.e_idx].plain_typ_for(is_own);
            let res_typ = cxt[self.e_idx].res_typ_id();
            cxt.lib_gen().zip_do_instantiate(
                quote!(Self::#acc_t_param),
                expr_typ,
                quote!(Self::#res_typ),
            )
        };

        let doc = doc::zipper_trait::fold_doc(cxt, self.e_idx, self.c_idx);

        quote! {
            #doc
            fn #id (
                &mut self,
                #( #params , )*
            ) -> #zip_do;
        }
    }

    pub fn to_call_tokens(&self, cxt: &cxt::ZipCxt, res: impl ToTokens) -> TokenStream {
        let id = &self.id;
        let params = self.params.index_iter().map(|(p_d_idx, param)| {
            let id = param.id();
            if p_d_idx == self.d_idx {
                let acc_field = cxt.lib_gen().coll_der_acc_field();
                quote!((#id.#acc_field, #res))
            } else {
                quote!(&#id)
            }
        });
        quote! {
            #id( #(#params ,)* )
        }
    }
}

#[derive(Debug, Clone)]
pub struct CollInitializer {
    e_idx: idx::Expr,
    v_idx: idx::Variant,
    d_idx: idx::Data,
    c_idx: idx::Coll,

    id: rust::Id,

    params: idx::DataMap<FnParam>,

    assoc_acc_typ: rust::Id,
}
impl CollInitializer {
    pub fn new(cxt: &cxt::FrameCxt, coll: &cxt::CollCxt, assoc_acc_typ: &rust::Id) -> Self {
        let assoc_acc_typ = assoc_acc_typ.clone();
        let e_idx = coll.e_idx();
        let v_idx = coll.v_idx();
        let d_idx = coll.d_idx();
        let c_idx = coll.c_idx();

        let e_cxt = &cxt[e_idx];
        let expr = e_cxt.expr();

        let id = gen::fun::initializer(
            e_cxt.e_id(),
            expr[v_idx].v_id(),
            expr[v_idx][d_idx].param_id(),
        );

        let params = expr[v_idx]
            .data()
            .index_iter()
            .map(|(p_d_idx, p_data)| {
                let id = p_data.param_id().clone();
                if p_d_idx < d_idx {
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.zip_res(cxt, true)),
                        rust::typ::reference(None, p_data.zip_res(cxt, false)),
                    )
                } else {
                    debug_assert!(p_d_idx >= d_idx);
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.frame_typ(cxt, true)),
                        rust::typ::reference(None, p_data.frame_typ(cxt, false)),
                    )
                }
            })
            .collect();

        Self {
            e_idx,
            v_idx,
            d_idx,
            c_idx,

            id,

            params,

            assoc_acc_typ,
        }
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }

    pub fn to_decl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let params = self.params.iter().map(|param| param.to_tokens(is_own));
        let zip_do = {
            let acc_t_param = &self.assoc_acc_typ;
            let expr_typ = cxt[self.e_idx].plain_typ_for(is_own);
            let res_typ = cxt[self.e_idx].res_typ_id();
            cxt.lib_gen().zip_do_instantiate(
                quote!(Self::#acc_t_param),
                expr_typ,
                quote!(Self::#res_typ),
            )
        };

        let doc = doc::zipper_trait::init_doc(cxt, self.e_idx, self.c_idx);

        quote! {
            #doc
            fn #id (
                &mut self,
                #( #params , )*
            ) -> #zip_do;
        }
    }

    pub fn to_call_tokens(&self) -> TokenStream {
        let id = &self.id;
        let params = self.params.iter().map(|param| param.id());
        quote! {
            #id( #(& #params ,)* )
        }
    }
}

pub type CollHandlers = idx::CollMap<CollHandler>;
impl CollHandlers {
    pub fn from(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        cxt[e_idx]
            .colls()
            .iter()
            .map(|coll| CollHandler::new(cxt, coll))
            .collect()
    }
    pub fn to_zipper_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let handlers = self
            .iter()
            .map(|handler| handler.to_zipper_trait_tokens(cxt, is_own));
        quote! {
            #(#handlers)*
        }
    }
}

#[derive(Debug, Clone)]
pub struct CollHandler {
    initializer: CollInitializer,
    folder: CollFolder,

    assoc_acc_typ: rust::Id,
}
impl CollHandler {
    pub fn new(cxt: &cxt::FrameCxt, coll: &cxt::CollCxt) -> Self {
        let assoc_acc_typ = coll.acc_t_param_id().clone();
        let initializer = CollInitializer::new(cxt, coll, &assoc_acc_typ);
        let folder = CollFolder::new(cxt, coll, &assoc_acc_typ);
        Self {
            initializer,
            folder,
            assoc_acc_typ,
        }
    }

    pub fn initializer(&self) -> &CollInitializer {
        &self.initializer
    }
    pub fn folder(&self) -> &CollFolder {
        &self.folder
    }
    pub fn assoc_acc_typ(&self) -> &rust::Id {
        &self.assoc_acc_typ
    }

    pub fn to_zipper_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let assoc_doc = doc::zipper_trait::assoc_typ_doc(cxt, self.folder.e_idx, self.folder.c_idx);
        let assoc = &self.assoc_acc_typ;
        let init = self.initializer.to_decl_tokens(cxt, is_own);
        let fold = self.folder.to_decl_tokens(cxt, is_own);
        quote! {
            #assoc_doc
            type #assoc;
            #init
            #fold
        }
    }
}

#[derive(Debug, Clone)]
pub struct ZipperTrait {
    e_idx: idx::Expr,
    id: rust::Id,

    own_generics: rust::Generics,
    ref_generics: rust::Generics,
}
impl ZipperTrait {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let e_cxt = &cxt[e_idx];

        let id = e_cxt.zip_trait_id().clone();
        let own_generics = e_cxt.generics().clone();
        let ref_generics = {
            // Add the expression lifetime if needed.
            let mut gen = own_generics.clone();

            let params_tail =
                std::mem::replace(&mut gen.params, syn::punctuated::Punctuated::new());
            gen.params.push(rust::typ::generic_param::from_lifetime(
                gen::lifetime::expr(),
            ));
            gen.params.extend(params_tail);

            gen
        };

        Self {
            e_idx,
            id,
            own_generics,
            ref_generics,
        }
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }

    pub fn generics(&self, is_own: IsOwn) -> &rust::Generics {
        if is_own {
            &self.own_generics
        } else {
            &self.ref_generics
        }
    }
}

impl ZipperTrait {
    pub fn to_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let (params, _, where_clause) = self.generics(is_own).split_for_impl();

        let expr_tokens = cxt[self.e_idx]
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|dep_e_idx| cxt[dep_e_idx].self_zip_trait_tokens(cxt, self.e_idx, is_own));

        let doc = doc::zipper_trait::doc(cxt, self.e_idx);

        // let macro_def = self.macro_def(cxt, is_own);

        quote! {
            #doc
            pub trait #id #params #where_clause {
                #(#expr_tokens)*
            }

            // #macro_def
        }
    }

    // pub fn macro_def(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
    //     let macro_id = if let Some(id) = cxt[self.e_idx].e_conf().impl_macro_name(is_own).deref() {
    //         id
    //     } else {
    //         return quote! {};
    //     };

    //     let (lft_side, rgt_side): (Vec<_>, Vec<_>) =
    //         cxt[self.e_idx]
    //             .fp_e_deps()
    //             .iter()
    //             .cloned()
    //             .map(|dep_e_idx| {
    //                 let e_cxt = &cxt[dep_e_idx];
    //                 let e_id = e_cxt.e_id();
    //                 let e_res = e_cxt.res_typ_id();

    //                 let t_params = e_cxt.generics().params.iter().enumerate().map(
    //                     |(index, param)| match param {
    //                         rust::GenericParam::Type(_) => (
    //                             rust::Id::new(&format!("t_param_{}", index), gen::span()),
    //                             false,
    //                         ),
    //                         rust::GenericParam::Lifetime(_) => (
    //                             rust::Id::new(&format!("t_param_{}", index), gen::span()),
    //                             true,
    //                         ),
    //                         rust::GenericParam::Const(_) => {
    //                             panic!("unexpected const type parameter in `{}`", e_id)
    //                         }
    //                     },
    //                 );

    //                 let e_binding = if e_cxt.generics().params.is_empty() {
    //                     quote!(#e_id)
    //                 } else {
    //                     let lft_t_params = t_params.clone().map(|(name, is_lt)| {
    //                         if is_lt {
    //                             quote! { $#name:lifetime }
    //                         } else {
    //                             quote! { $#name:ty }
    //                         }
    //                     });
    //                     quote!(#e_id< #(#lft_t_params)* >)
    //                 };

    //                 let e_typ = {
    //                     let t_params = t_params.clone().map(|(id, _)| id);
    //                     quote!(#e_id < #($#t_params),* >)
    //                 };

    //                 let inspect_expr_param =
    //                     rust::Id::new(&format!("{}_expr_param", e_id), gen::span());
    //                 let inspect_def = gen::ZipIds::inspect_fun(e_id);
    //                 let inspect_sig = e_cxt.fun_inspect_self_sig_tokens(
    //                     cxt,
    //                     is_own,
    //                     &quote!($#inspect_expr_param),
    //                     Some(e_typ),
    //                 );

    //                 let (variants_lft, variants_rgt): (Vec<_>, Vec<_>) = e_cxt
    //                     .expr()
    //                     .variants()
    //                     .iter()
    //                     .map(|variant| {
    //                         let v_id = variant.v_id();
    //                         let fields = variant.data().iter().map(|data| {
    //                             rust::Id::new(
    //                                 &format!("{}_{}_{}", e_id, v_id, data.param_id()),
    //                                 gen::span(),
    //                             )
    //                         });

    //                         let lft_binding = match variant.is_struct_like() {
    //                             Some(true) => quote! {
    //                                 #v_id { #($#fields:pat),* $(,)? }
    //                             },
    //                             Some(false) => quote! {
    //                                 #v_id ( #($#fields:pat),* $(,)? )
    //                             },
    //                             None => quote! { #v_id },
    //                         };

    //                         let handler =
    //                             &self.variant_handlers.get(&dep_e_idx).unwrap()[variant.v_idx()];
    //                         let def_id = &handler.go_up_id;

    //                         (
    //                             quote! {
    //                                 #lft_binding => {
    //                                     go_up: $#def_id:expr $(,)?
    //                                 }
    //                             },
    //                             quote! {},
    //                         )
    //                     })
    //                     .unzip();

    //                 (
    //                     quote! {
    //                         #e_binding where Res = $#e_res:ty {
    //                             variants: |&mut self| {
    //                                 #(#variants_lft $(,)?)*
    //                             }

    //                             $(
    //                                 ,
    //                                 inspect: |&mut self, $#inspect_expr_param:pat $(,)?|
    //                                 $#inspect_def:expr
    //                                 $(,)?
    //                             )?
    //                         }
    //                     },
    //                     quote! {
    //                         type #e_res = $#e_res;

    //                         #(#variants_rgt)*

    //                         $(
    //                             #inspect_sig {
    //                                 $#inspect_def
    //                             }
    //                         )?
    //                     },
    //                 )
    //             })
    //             .unzip();

    //     let error_msg =
    //         format!("you are not using this macro correctly, please refer to its documentation");

    //     quote_spanned! { macro_id.span() =>
    //         #[macro_export]
    //         macro_rules! #macro_id {
    //             (
    //                 #(#lft_side)*
    //             ) => {
    //                 #(#rgt_side)*
    //             };
    //             ($($stuff:tt)*) => {
    //                 compile_error! { #error_msg }
    //             };
    //         }
    //     }
    // }
}
