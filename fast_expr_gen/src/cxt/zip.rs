use super::*;

pub type Infos = idx::ExprMap<Info>;
pub struct Info {
    zip_struct: ZipStruct,
    zipper_trait: ZipperTrait,
}
impl Info {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let zip_struct = ZipStruct::new(cxt, e_idx);
        let zipper_trait = ZipperTrait::new(cxt, e_idx);
        Self {
            zip_struct,
            zipper_trait,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZipField {
    e_idx: idx::Expr,
    id: rust::Id,

    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl ZipField {
    pub fn new(e_cxt: &cxt::frames::ECxt) -> Option<Self> {
        e_cxt.frames().map(|frames| {
            let e_idx = e_cxt.e_idx();
            let id = gen::field::expr_zip::stack(e_cxt.id().clone());

            let own_typ = frames.plain_typ(true);
            let ref_typ = frames.plain_typ(false);

            Self {
                e_idx,
                id,
                own_typ,
                ref_typ,
            }
        })
    }

    pub fn typ(&self, is_own: IsOwn) -> &rust::Typ {
        if is_own {
            &self.own_typ
        } else {
            &self.ref_typ
        }
    }
}

impl ZipField {
    pub fn to_field_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let typ = self.typ(is_own);
        syn::parse_quote!(pub #id: std::vec::Vec<#typ>)
    }
}

#[derive(Debug, Clone)]
pub struct ZipStruct {
    e_idx: idx::Expr,
    id: rust::Id,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,

    my_stack: Option<rust::Id>,

    stepper_field_id: rust::Id,
    stepper_field_typ: rust::Id,
    stacks: Map<idx::Expr, ZipField>,

    sink_id: rust::Id,
}
impl ZipStruct {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let e_cxt = &cxt[e_idx];
        let id = gen::typ::zip(e_cxt.id().clone());

        let stepper_t_param = gen::typ::param::step();
        let stepper_field_id = gen::field::expr_zip::stepper();
        let stepper_field_typ = stepper_t_param.clone();

        let new_generics = |is_own: bool| {
            let mut generics = rust::Generics {
                lt_token: Some(syn::token::Lt::default()),
                params: syn::punctuated::Punctuated::new(),
                gt_token: Some(syn::token::Gt::default()),
                where_clause: None,
            };

            if !is_own {
                let lt = gen::lifetime::expr();
                generics
                    .params
                    .push(rust::typ::generic_param::from_lifetime(lt.clone()));
            }

            let expr_generics = e_cxt.generics();
            generics.params.extend(expr_generics.params.iter().cloned());
            debug_assert_eq!(generics.where_clause, None);
            generics.where_clause = expr_generics.where_clause.clone();

            for dep_e_idx in e_cxt.fp_e_deps().iter().cloned() {
                let dep_e_cxt = &cxt[dep_e_idx];
                let res_id = dep_e_cxt.res_typ_id().clone();
                generics
                    .params
                    .push(rust::typ::generic_param::from_id(res_id));
                for coll in dep_e_cxt.colls() {
                    let acc_id = coll.acc_t_param_id().clone();
                    generics
                        .params
                        .push(rust::typ::generic_param::from_id(acc_id));
                }
            }

            // Zipper type parameter and bounds.
            generics
                .params
                .push(rust::typ::generic_param::from_id(stepper_t_param.clone()));

            let dep_typ_constraints = e_cxt.fp_e_deps().iter().cloned().map(|dep_e_idx| {
                let dep_e_cxt = &cxt[dep_e_idx];
                let dep_res = dep_e_cxt.res_typ_id();
                let colls = dep_e_cxt.colls().iter().map(|coll| coll.acc_t_param_id());

                quote! {
                    #dep_res = #dep_res,
                    #( #colls = #colls, )*
                }
            });

            let zip_trait = e_cxt.zip_trait_id();
            let expr_params = e_cxt.generics().params.iter().map(|param| match param {
                rust::GenericParam::Type(typ_param) => typ_param.ident.to_token_stream(),
                rust::GenericParam::Lifetime(lt_def) => lt_def.lifetime.to_token_stream(),
                rust::GenericParam::Const(const_param) => const_param.ident.to_token_stream(),
            });

            // let tokens = quote! {
            //     #stepper_t_param : #zip_trait<
            //         #( #expr_params, )* #(#dep_typ_constraints)*
            //     >,
            // };
            generics
                .make_where_clause()
                .predicates
                .push(syn::parse_quote! {
                    #stepper_t_param : #zip_trait<
                        #( #expr_params, )* #(#dep_typ_constraints)*
                    >
                });

            generics
        };

        let mut my_stack = None;

        let mut stacks = Map::new();
        for other_e_idx in e_cxt.fp_e_deps().iter().cloned() {
            if let Some(field) = ZipField::new(&cxt[other_e_idx]) {
                if e_idx == other_e_idx {
                    my_stack = Some(field.id.clone())
                }
                let prev = stacks.insert(other_e_idx, field);
                debug_assert_eq!(prev, None);
            }
        }

        let sink_id = gen::field::expr_zip::sink();

        Self {
            e_idx,
            id,
            own_generics: new_generics(true),
            ref_generics: new_generics(false),

            my_stack,

            stepper_field_id,
            stepper_field_typ,

            stacks,
            sink_id,
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
    pub fn stacks(&self) -> &Map<idx::Expr, ZipField> {
        &self.stacks
    }

    pub fn zip_field(&self) -> TokenStream {
        let zip_field = &self.stepper_field_id;
        quote!(self.#zip_field)
    }

    pub fn sink_typ(&self, is_own: IsOwn) -> rust::Typ {
        let generics = self.generics(is_own);
        let lifetimes = generics.lifetimes().map(|lt_def| &lt_def.lifetime);
        let typs = generics.type_params().map(|typ_param| &typ_param.ident);
        syn::parse_quote! {
            std::marker::PhantomData <(
                #( & #lifetimes (), )*
                #( #typs, )*
            )>
        }
    }
}

impl ZipStruct {
    pub fn to_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let def = self.struct_def_tokens(is_own);
        let imp = self.struct_impl_tokens(cxt, is_own);
        quote! { #def #imp }
    }

    pub fn struct_def_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let generics = self.generics(is_own);
        let (params, _, where_clause) = generics.split_for_impl();

        let stepper_id = &self.stepper_field_id;
        let stepper_typ = &self.stepper_field_typ;

        let stacks = self
            .stacks
            .values()
            .map(|stack| stack.to_field_tokens(is_own));

        let sink_id = &self.sink_id;
        let sink_typ = self.sink_typ(is_own);

        quote! {
            pub struct #id #params #where_clause {
                pub #stepper_id: #stepper_typ,
                #(#stacks ,)*
                pub #sink_id: #sink_typ,
            }
        }
    }

    pub fn struct_impl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = self.id();
        let zip_fn_tokens = self.zip_fns_tokens(cxt, is_own);
        let generics = self.generics(is_own);
        let (impl_params, typ_params, where_clause) = generics.split_for_impl();
        let constructors = self.constructors();
        let variant_handlers = self.variant_handlers(cxt, is_own);
        let expr_handlers = self.expr_handlers(cxt, is_own);
        let frame_handlers = self.frame_handlers(cxt, is_own);

        quote! {
            impl #impl_params #id #typ_params #where_clause {
                #constructors
                #( #zip_fn_tokens )*
                #( #variant_handlers )*
                #( #expr_handlers )*
                #( #frame_handlers )*
            }
        }
    }

    pub fn constructors(&self) -> TokenStream {
        let stepper_id = &self.stepper_field_id;
        let stepper_typ = &self.stepper_field_typ;
        let capa = rust::Id::new("capacity", gen::span());
        let capa_typ = rust::Id::new("usize", gen::span());
        let init_stacks = self.stacks.values().map(|stack| {
            let id = &stack.id;
            quote!(#id: std::vec::Vec::new())
        });
        let init_stacks_capa = self.stacks.values().map(|stack| {
            let id = &stack.id;
            quote!(#id: std::vec::Vec::with_capacity(#capa))
        });
        let sink_id = &self.sink_id;
        quote! {
            pub fn new(#stepper_id: #stepper_typ) -> Self {
                Self {
                    #stepper_id,
                    #(#init_stacks,)*
                    #sink_id: std::marker::PhantomData,
                }
            }
            pub fn with_capacity(#stepper_id: #stepper_typ, #capa: #capa_typ) -> Self {
                Self {
                    #stepper_id,
                    #(#init_stacks_capa,)*
                    #sink_id: std::marker::PhantomData,
                }
            }
        }
    }

    pub fn variant_handlers<'a>(
        &self,
        cxt: &'a cxt::ZipCxt,
        is_own: IsOwn,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        cxt[self.e_idx]
            .fp_e_deps()
            .iter()
            .cloned()
            .map(move |dep_e_idx| {
                cxt[dep_e_idx]
                    .expr()
                    .variants()
                    .iter()
                    .map(move |variant| variant.to_zip_handler_fn_tokens(cxt, is_own))
            })
            .flatten()
    }

    pub fn expr_handlers<'a>(
        &self,
        cxt: &'a cxt::ZipCxt,
        is_own: IsOwn,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        cxt[self.e_idx]
            .fp_e_deps()
            .iter()
            .cloned()
            .map(move |dep_e_idx| cxt[dep_e_idx].to_zip_handler_fn_tokens(cxt, is_own))
    }

    pub fn frame_handlers<'a>(
        &self,
        cxt: &'a cxt::ZipCxt,
        is_own: IsOwn,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        cxt[self.e_idx]
            .fp_e_deps()
            .iter()
            .cloned()
            .filter_map(move |dep_e_idx| {
                cxt[dep_e_idx]
                    .frames()
                    .map(move |frame| frame.to_zip_frame_handler_fn_tokens(cxt, is_own))
            })
    }

    pub fn zip_fn_tokens<'a>(
        &'a self,
        cxt: &'a cxt::ZipCxt,
        zip_e_idx: idx::Expr,
        is_own: IsOwn,
    ) -> TokenStream {
        let e_cxt = &cxt[self.e_idx];
        let zip_e_cxt = &cxt[zip_e_idx];
        let fun_id = zip_e_cxt.zip_fun_id();
        let expr_typ = zip_e_cxt.plain_typ_for(is_own);
        let res_typ = zip_e_cxt.res_typ();
        let expr_id = gen::zip::expr_var();
        let new_expr_id = gen::zip::new_expr_var();
        let zip_field = self.zip_field();
        let handle_expr = {
            let expr_handler_fn = gen::fun::expr_handler(cxt[zip_e_idx].id());
            quote!(self.#expr_handler_fn)
        };
        let res_var = gen::zip::res_var();

        let sig = quote! {
            pub fn #fun_id(&mut self, mut #expr_id: #expr_typ) -> #res_typ
        };

        if let Some(my_stack) = zip_e_cxt.zip_struct().my_stack.as_ref() {
            let my_stack = quote!(self.#my_stack);
            let handle_frame = {
                let handler = gen::fun::frame_handler(zip_e_cxt.id());
                quote!(self.#handler)
            };
            let depth_on_entry_id = gen::zip::depth_var();
            let zip_do = gen::zip::zip_do_var();

            let inspect = e_cxt
                .zipper_trait()
                .inspecters
                .get(&zip_e_idx)
                .unwrap_or_else(|| {
                    panic!(
                        "expression type {} has no inspecter for {}",
                        e_cxt.id(),
                        zip_e_cxt.id()
                    )
                })
                .id();

            let zip_do_down_and_then = gen::lib::zip_do::down_and_then();
            let frame_var = gen::zip::frame_var();
            let new_res_var = gen::zip::new_res_var();

            let drain_stack = quote! {
                #my_stack.drain(#depth_on_entry_id ..)
            };

            let zip_do_cases = gen::lib::zip_do::match_cases(
                // down
                quote! { (#frame_var, #new_expr_id) },
                quote! {{
                    #expr_id = #new_expr_id;
                    #my_stack.push(#frame_var);
                    continue 'go_down;
                }},
                // up
                quote! { #res_var },
                quote! { #res_var },
                // subst
                quote! { #new_expr_id },
                quote! {{
                    #expr_id = #new_expr_id;
                    continue 'go_down;
                }},
                // early
                quote! { #res_var },
                quote! {{
                    #drain_stack;
                    return #res_var;
                }},
            );

            let frame_zip_do_cases = gen::lib::zip_do::match_cases(
                // down
                quote! { (#frame_var, #new_expr_id) },
                quote! {{
                    #expr_id = #new_expr_id;
                    #my_stack.push(#frame_var);
                    continue 'go_down;
                }},
                // up
                quote! { #new_res_var },
                quote! {{
                    #res_var = #new_res_var;
                    continue 'go_up;
                }},
                // subst
                quote! { #new_expr_id },
                quote! {{
                    #expr_id = #new_expr_id;
                    continue 'go_down;
                }},
                // early
                quote! { #res_var },
                quote! {{
                    #drain_stack;
                    return #res_var;
                }},
            );

            let go_down_body = {
                quote! {
                    let #zip_do = #zip_field.#inspect(#expr_id).#zip_do_down_and_then(
                        |#new_expr_id| #handle_expr(#new_expr_id)
                    );

                    let mut #res_var = match #zip_do {
                        #zip_do_cases
                    };

                    'go_up: loop {
                        debug_assert!(#depth_on_entry_id <= #my_stack.len());

                        if #depth_on_entry_id == #my_stack.len() {
                            break 'go_up;
                        }

                        let #frame_var = if let Some(frame) = #my_stack.pop() {
                            frame
                        } else {
                            unreachable!(
                                "[fast_expr] depth on entry is {}, \
                                stack length is {}, \
                                we should have `stack length > depth on entry`, \
                                but stack is empty",
                                #depth_on_entry_id, #my_stack.len(),
                            )
                        };

                        match #handle_frame(#res_var, #frame_var) {
                            #frame_zip_do_cases
                        }
                    }

                    debug_assert!(#depth_on_entry_id == #my_stack.len());
                    return #res_var;
                }
            };

            quote! {
                #sig {
                    let #depth_on_entry_id = #my_stack.len();

                    'go_down: loop {
                        debug_assert!(#depth_on_entry_id <= #my_stack.len());

                        #go_down_body
                    }
                }
            }
        } else {
            let zip_do_cases = gen::lib::zip_do::match_cases(
                // down
                quote! { empty },
                quote! {{
                    match empty {}
                }},
                // up
                quote! { #res_var },
                quote! {{
                    return #res_var;
                }},
                // subst
                quote! { #new_expr_id },
                quote! {{
                    #expr_id = #new_expr_id;
                    continue 'subst;
                }},
                // early
                quote! { #res_var },
                quote! {{
                    return #res_var;
                }},
            );

            quote! {
                #sig {
                    'subst: loop {
                        match #handle_expr(#expr_id) {
                            #zip_do_cases
                        }
                    }
                }
            }
        }
    }

    pub fn zip_fns_tokens<'a>(
        &'a self,
        cxt: &'a cxt::ZipCxt,
        is_own: IsOwn,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        let e_cxt = &cxt[self.e_idx];

        e_cxt
            .fp_e_deps()
            .iter()
            .cloned()
            .map(move |dep_e_idx| self.zip_fn_tokens(cxt, dep_e_idx, is_own))
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    id: rust::Id,
    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl FnParam {
    pub fn from_data(e_cxt: &cxt::frames::ECxt, data: &expr::Data) -> Self {
        let id = gen::fun::param::data_param(
            data.id()
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
    go_up_lt_param: Option<rust::Lifetime>,
    go_up_params: idx::DataMap<FnParam>,
    go_up_res: rust::Typ,
}
impl VariantHandler {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr, variant: &expr::Variant) -> Self {
        let v_idx = variant.v_idx();

        let go_up_id = variant.zipper_go_up_id().clone();
        let go_up_lt_param = if variant.contains_leaf_data() {
            Some(gen::lifetime::expr())
        } else {
            None
        };
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
            go_up_lt_param,
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
        let lt_opt = if is_own {
            None
        } else {
            self.go_up_lt_param.as_ref()
        };
        quote! {
            fn #id <#lt_opt> (&mut self, #(#params,)* ) -> #res;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Inspecter {
    e_idx: idx::Expr,
    id: rust::Id,
    own_typ: rust::Typ,
    ref_typ: rust::Typ,
    res_typ: rust::Typ,
}
impl Inspecter {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let id = gen::fun::inspect(cxt[e_idx].id());
        let own_typ = cxt[e_idx].plain_typ_for(true);
        let ref_typ = cxt[e_idx].plain_typ_for(false);
        let res_typ = {
            let res = cxt[e_idx].res_typ_id();
            syn::parse_quote!(Self :: #res)
        };
        Self {
            e_idx,
            id,
            own_typ,
            ref_typ,
            res_typ,
        }
    }

    pub fn id(&self) -> &rust::Id {
        &self.id
    }

    pub fn to_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let t_params = if is_own {
            None
        } else {
            Some(gen::lifetime::expr())
        };
        let expr_id = gen::zip::expr_var();
        let e_typ = if is_own { &self.own_typ } else { &self.ref_typ };
        let res = &self.res_typ;
        let zip_do = gen::lib::zip_do::instantiate(&e_typ, &e_typ, &res);
        let build_down = gen::lib::zip_do::new_go_down(&expr_id);

        quote! {
            fn #id < #t_params > (
                &mut self, #expr_id: #e_typ
            ) -> #zip_do {
                #build_down
            }
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

        let id = gen::fun::folder(e_cxt.id(), expr[v_idx].id(), expr[v_idx][d_idx].param_id());

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
        let lt = if is_own {
            None
        } else {
            Some(gen::lifetime::expr())
        };
        quote! {
            fn #id <#lt> (
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

        let id = gen::fun::initializer(e_cxt.id(), expr[v_idx].id(), expr[v_idx][d_idx].param_id());

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
        let lt = if is_own {
            None
        } else {
            Some(gen::lifetime::expr())
        };
        quote! {
            fn #id <#lt> (
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
    generics: rust::Generics,

    assoc_typs: Vec<rust::Id>,

    variant_handlers: idx::VariantMap<VariantHandler>,
    inspecters: Map<idx::Expr, Inspecter>,
    initializers: idx::ExprMap<idx::CollMap<CollInitializer>>,
    folders: idx::ExprMap<idx::CollMap<CollFolder>>,
}
impl ZipperTrait {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let e_cxt = &cxt[e_idx];

        let id = e_cxt.zip_trait_id().clone();
        let generics = e_cxt.generics().clone();

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

        let inspecters = e_cxt
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|dep_e_idx| (dep_e_idx, Inspecter::new(cxt, dep_e_idx)))
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
            generics,

            assoc_typs,

            variant_handlers,
            inspecters,
            initializers,
            folders,
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
        let (params, _, where_clause) = self.generics.split_for_impl();
        let assoc_typs = &self.assoc_typs;

        let handlers = self
            .variant_handlers
            .iter()
            .map(|handler| handler.to_go_up_tokens(is_own));

        let inspecters = self
            .inspecters
            .values()
            .map(|inspecter| inspecter.to_tokens(is_own));

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

#[derive(Debug, Clone)]
pub struct ECxt {
    cxt: cxt::frames::ECxt,
    zip_struct: ZipStruct,
    zipper_trait: ZipperTrait,
    zip_fun_id: rust::Id,
}
implement! {
    impl ECxt {
        Deref<cxt::frames::ECxt>, DerefMut {
            field: cxt
        }
    }
}

impl ECxt {
    pub fn new(
        cxt: cxt::frames::ECxt,
        Info {
            zip_struct,
            zipper_trait,
        }: Info,
    ) -> Self {
        let zip_fun_id = gen::fun::zip(cxt.id());
        Self {
            cxt,
            zip_struct,
            zipper_trait,
            zip_fun_id,
        }
    }

    pub fn zip_fun_id(&self) -> &rust::Id {
        &self.zip_fun_id
    }

    pub fn zip_struct(&self) -> &ZipStruct {
        &self.zip_struct
    }
    pub fn zipper_trait(&self) -> &ZipperTrait {
        &self.zipper_trait
    }
}

impl ECxt {
    pub fn zip_mod_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let mut tokens = self.frame_enum_tokens(is_own);
        tokens.extend(self.zip_trait_tokens(cxt, is_own));
        tokens.extend(self.zip_struct_tokens(cxt, is_own));
        tokens
    }

    pub fn zip_struct_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zip_struct.to_tokens(cxt, is_own)
    }

    pub fn zip_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zipper_trait.to_tokens(cxt, is_own)
    }
}

impl ECxt {
    pub fn zip_variant_handler_out_typ(&self, is_own: IsOwn) -> TokenStream {
        let expr = self.plain_typ_for(is_own);
        let res = self.res_typ_id();

        let down = if let Some(frames) = self.frames() {
            let frame_typ = frames.plain_typ(is_own);
            quote!( ( #frame_typ, #expr ) )
        } else {
            gen::lib::empty::instantiate()
        };
        gen::lib::zip_do::instantiate(&down, &expr, &res)
    }

    pub fn to_zip_handler_fn_tokens(&self, _cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let fn_id = gen::fun::expr_handler(self.id());
        let expr_var = gen::zip::expr_var();
        let expr_typ = self.plain_typ_for(is_own);
        let out_typ = self.zip_variant_handler_out_typ(is_own);

        let match_cases =
            self.expr()
                .to_variant_constructors_tokens()
                .map(|(v_idx, constructor)| {
                    let variant_handler =
                        gen::fun::variant_handler(self.id(), self.expr()[v_idx].id());
                    let params = self.expr().variants()[v_idx]
                        .data()
                        .iter()
                        .map(|data| data.param_id());
                    quote! {
                        #constructor => self.#variant_handler( #(#params, )* )
                    }
                });

        quote! {
            pub fn #fn_id(&mut self, #expr_var: #expr_typ) -> #out_typ {
                match #expr_var {
                    #(#match_cases ,)*
                }
            }
        }
    }
}
