//! Handles data specific to the zipper trait and its implementations.

prelude! {}

#[derive(Debug, Clone)]
pub struct FnParam {
    id: rust::Id,
    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl FnParam {
    pub fn from_data(e_cxt: &cxt::frame::ECxt, data: &expr::Data) -> Self {
        let id = gen::fun::param::data_param(
            data.d_id()
                .map(Either::Left)
                .unwrap_or_else(|| Either::Right(data.d_idx())),
        );
        let own_typ = data.zip_res(e_cxt, true);
        let ref_typ = data.zip_res(e_cxt, false);
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
pub struct VariantHandler {
    e_idx: idx::Expr,
    v_idx: idx::Variant,

    go_up_id: rust::Id,
    go_up_params: idx::DataMap<FnParam>,
    go_up_res: rust::Typ,
}
impl VariantHandler {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr, variant: &expr::Variant) -> Self {
        let v_idx = variant.v_idx();

        let go_up_id = variant.zipper_go_up_id().clone();
        let go_up_params = variant
            .data()
            .iter()
            .map(|data| FnParam::from_data(&cxt[e_idx], data))
            .collect();
        let go_up_res = {
            let typ = cxt[e_idx].res_typ_id();
            syn::parse_quote!(Self :: #typ)
        };

        Self {
            e_idx,
            v_idx,

            go_up_id,
            go_up_params,
            go_up_res,
        }
    }

    pub fn to_go_up_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.go_up_id;
        let params = self
            .go_up_params
            .iter()
            .map(|param| param.to_tokens(is_own));
        let res = &self.go_up_res;
        quote! {
            fn #id (&mut self, #(#params,)* ) -> #res;
        }
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
}

impl CollFolder {
    pub fn new(cxt: &cxt::FrameCxt, coll: &cxt::CollCxt) -> Self {
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
                        rust::typ::reference(None, p_data.zip_res(e_cxt, true)),
                        rust::typ::reference(None, p_data.zip_res(e_cxt, false)),
                    )
                } else if p_d_idx == d_idx {
                    let acc = e_cxt.colls()[c_idx].acc_t_param_id();
                    let inner = p_data
                        .inner()
                        .expect("trying to build a folder over non-recursive data");
                    let res = cxt[inner].res_typ_id();
                    let typ: rust::Typ = syn::parse_quote! {
                        (Self::#acc, Self::#res)
                    };
                    FnParam::new(id, typ.clone(), typ)
                } else {
                    debug_assert!(p_d_idx > d_idx);
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.frame_typ(e_cxt, true)),
                        rust::typ::reference(None, p_data.frame_typ(e_cxt, false)),
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
        }
    }

    pub fn to_decl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let params = self.params.iter().map(|param| param.to_tokens(is_own));
        let zip_do = {
            let acc_t_param = cxt[self.e_idx].colls()[self.c_idx].acc_t_param_id();
            let expr_typ = cxt[self.e_idx].plain_typ_for(is_own);
            let res_typ = cxt[self.e_idx].res_typ_id();
            gen::lib::zip_do::instantiate(
                quote!(Self::#acc_t_param),
                expr_typ,
                quote!(Self::#res_typ),
            )
        };
        quote! {
            fn #id (
                &mut self,
                #( #params , )*
            ) -> #zip_do;
        }
    }

    pub fn to_call_tokens(&self, res: impl ToTokens) -> TokenStream {
        let id = &self.id;
        let params = self.params.index_iter().map(|(p_d_idx, param)| {
            let id = param.id();
            if p_d_idx == self.d_idx {
                let acc_field = gen::lib::coll_der::acc_field();
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
}
impl CollInitializer {
    pub fn new(cxt: &cxt::FrameCxt, coll: &cxt::CollCxt) -> Self {
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
                        rust::typ::reference(None, p_data.zip_res(e_cxt, true)),
                        rust::typ::reference(None, p_data.zip_res(e_cxt, false)),
                    )
                } else {
                    debug_assert!(p_d_idx >= d_idx);
                    FnParam::new(
                        id,
                        rust::typ::reference(None, p_data.frame_typ(e_cxt, true)),
                        rust::typ::reference(None, p_data.frame_typ(e_cxt, false)),
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
        }
    }

    pub fn to_decl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let params = self.params.iter().map(|param| param.to_tokens(is_own));
        let zip_do = {
            let acc_t_param = cxt[self.e_idx].colls()[self.c_idx].acc_t_param_id();
            let expr_typ = cxt[self.e_idx].plain_typ_for(is_own);
            let res_typ = cxt[self.e_idx].res_typ_id();
            gen::lib::zip_do::instantiate(
                quote!(Self::#acc_t_param),
                expr_typ,
                quote!(Self::#res_typ),
            )
        };
        quote! {
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

#[derive(Debug, Clone)]
pub struct ZipperTrait {
    e_idx: idx::Expr,
    id: rust::Id,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,

    assoc_typs: Vec<rust::Id>,

    variant_handlers: idx::VariantMap<VariantHandler>,
    initializers: idx::ExprMap<idx::CollMap<CollInitializer>>,
    folders: idx::ExprMap<idx::CollMap<CollFolder>>,
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

        let assoc_typs = e_cxt
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|dep_e_idx| {
                Some(cxt[dep_e_idx].res_typ_id().clone()).into_iter().chain(
                    cxt[dep_e_idx]
                        .colls()
                        .iter()
                        .map(|coll| coll.acc_t_param_id().clone()),
                )
            })
            .flatten()
            .collect();

        let variant_handlers = e_cxt
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|dep_e_idx| {
                cxt[dep_e_idx]
                    .expr()
                    .variants()
                    .iter()
                    .map(move |variant| VariantHandler::new(cxt, dep_e_idx, variant))
            })
            .flatten()
            .collect();

        let initializers = cxt
            .e_cxts()
            .iter()
            .map(|e_cxt| {
                e_cxt
                    .colls()
                    .iter()
                    .map(|coll| CollInitializer::new(cxt, coll))
                    .collect()
            })
            .collect();

        let folders = cxt
            .e_cxts()
            .iter()
            .map(|e_cxt| {
                e_cxt
                    .colls()
                    .iter()
                    .map(|coll| CollFolder::new(cxt, coll))
                    .collect()
            })
            .collect();

        Self {
            e_idx,
            id,
            own_generics,
            ref_generics,

            assoc_typs,

            variant_handlers,
            initializers,
            folders,
        }
    }

    pub fn generics(&self, is_own: IsOwn) -> &rust::Generics {
        if is_own {
            &self.own_generics
        } else {
            &self.ref_generics
        }
    }

    pub fn variant_handlers(&self) -> &idx::VariantMap<VariantHandler> {
        &self.variant_handlers
    }
    pub fn coll_initializers(&self) -> &idx::ExprMap<idx::CollMap<CollInitializer>> {
        &self.initializers
    }
    pub fn coll_folders(&self) -> &idx::ExprMap<idx::CollMap<CollFolder>> {
        &self.folders
    }
}

impl ZipperTrait {
    pub fn to_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let (params, _, where_clause) = self.generics(is_own).split_for_impl();
        let assoc_typs = &self.assoc_typs;

        let handlers = self
            .variant_handlers
            .iter()
            .map(|handler| handler.to_go_up_tokens(is_own));

        let inspecters = cxt[self.e_idx]
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|dep_e_idx| cxt[dep_e_idx].fun_inspect_self_def_tokens(is_own));

        let initializers = self
            .initializers
            .iter()
            .map(|initializers| {
                initializers
                    .iter()
                    .map(|initializer| initializer.to_decl_tokens(cxt, is_own))
            })
            .flatten();

        let folders = self
            .folders
            .iter()
            .map(|folders| {
                folders
                    .iter()
                    .map(|folder| folder.to_decl_tokens(cxt, is_own))
            })
            .flatten();

        quote! {
            pub trait #id #params #where_clause {
                #(type #assoc_typs;)*

                #(#handlers)*

                #(#inspecters)*

                #(#initializers)*

                #(#folders)*
            }
        }
    }
}
