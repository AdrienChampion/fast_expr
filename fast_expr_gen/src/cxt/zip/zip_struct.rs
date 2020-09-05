//! Handles the data specific to the zip struct and its implementation(s).

prelude! {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZipField {
    e_idx: idx::Expr,
    id: rust::Id,

    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl ZipField {
    pub fn new(e_cxt: &cxt::frame::ECxt) -> Option<Self> {
        e_cxt.frames().map(|frames| {
            let e_idx = e_cxt.e_idx();
            let id = gen::ZipIds::stack_field(e_cxt.e_id());

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
        syn::parse_quote!(#id: std::vec::Vec<#typ>)
    }
}

#[derive(Debug, Clone)]
pub struct ZipStruct {
    e_idx: idx::Expr,
    id: rust::Id,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,

    my_stack: Option<rust::Id>,

    stepper_field_typ: rust::Id,
    stacks: Map<idx::Expr, ZipField>,

    sink_id: rust::Id,
}
impl ZipStruct {
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let e_cxt = &cxt[e_idx];
        let id = gen::typ::zip(e_cxt.e_id().clone());

        let stepper_t_param = gen::typ::param::step();
        let stepper_field_typ = stepper_t_param.clone();

        let new_generics = |is_own: bool| {
            let mut generics = rust::Generics {
                lt_token: Some(rust::token::Lt::default()),
                params: syn::punctuated::Punctuated::new(),
                gt_token: Some(rust::token::Gt::default()),
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

            let expr_lt = if is_own {
                None
            } else {
                let lt = gen::lifetime::expr();
                Some(quote!(#lt ,))
            };

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
                        #expr_lt #( #expr_params, )* #(#dep_typ_constraints)*
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
        let def = self.struct_def_tokens(cxt, is_own);
        let imp = self.struct_impl_tokens(cxt, is_own);
        quote! { #def #imp }
    }

    pub fn struct_def_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let generics = self.generics(is_own);
        let (params, _, where_clause) = generics.split_for_impl();

        let step_field = &cxt.zip_ids().step_field;
        let stepper_typ = &self.stepper_field_typ;

        let stacks = self
            .stacks
            .values()
            .map(|stack| stack.to_field_tokens(is_own));

        let sink_id = &self.sink_id;
        let sink_typ = self.sink_typ(is_own);

        let vis = cxt.conf().secret_item_vis();

        quote! {
            pub struct #id #params #where_clause {
                #vis #step_field: #stepper_typ,
                #(#vis #stacks ,)*
                #vis #sink_id: #sink_typ,
            }
        }
    }

    pub fn struct_impl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = self.id();
        let zip_fn_tokens = self.zip_fns_tokens(cxt, is_own);
        let generics = self.generics(is_own);
        let (impl_params, typ_params, where_clause) = generics.split_for_impl();
        let constructors = self.constructors(cxt);
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

    pub fn constructors(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        let step_field = &cxt.zip_ids().step_field;
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
            pub fn new(#step_field: #stepper_typ) -> Self {
                Self {
                    #step_field,
                    #(#init_stacks,)*
                    #sink_id: std::marker::PhantomData,
                }
            }
            pub fn with_capacity(#step_field: #stepper_typ, #capa: #capa_typ) -> Self {
                Self {
                    #step_field,
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
            .map(move |dep_e_idx| cxt[dep_e_idx].self_zip_fun_def_tokens(cxt, is_own))
    }
}
