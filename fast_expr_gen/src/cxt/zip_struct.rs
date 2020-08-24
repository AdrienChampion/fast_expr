use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZipField {
    e_idx: idx::Expr,
    id: rust::Id,

    own_typ: rust::Typ,
    ref_typ: rust::Typ,
}
impl ZipField {
    pub fn new(e_cxt: &cxt::with_frames::ECxt) -> Self {
        let e_idx = e_cxt.e_idx();
        let id = gen::field::expr_zip::stack(e_cxt.id().clone());

        let frame_id = e_cxt.frame_typ_id();
        let (_, own_frame_params, _) = e_cxt.frame_generics(true).split_for_impl();
        let (_, ref_frame_params, _) = e_cxt.frame_generics(false).split_for_impl();

        let own_typ = syn::parse_quote!(#frame_id #own_frame_params);
        let ref_typ = syn::parse_quote!(#frame_id #ref_frame_params);

        Self {
            e_idx,
            id,
            own_typ,
            ref_typ,
        }
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
        syn::parse_quote!(#id: #typ)
    }
}

#[derive(Debug, Clone)]
pub struct ZipStruct {
    id: rust::Id,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,

    zipper_field_id: rust::Id,
    zipper_field_typ: rust::Id,
    stacks: Map<idx::Expr, ZipField>,
}
impl ZipStruct {
    pub fn nu_new(e_idx: idx::Expr, cxt: &FrameCxt) -> Self {
        let my_e_cxt = &cxt[e_idx];
        let id = my_e_cxt.id().clone();

        let zip_param = gen::typ::param::zip();
        let zipper_field_id = gen::field::expr_zip::zipper();
        let zipper_field_typ = zip_param.clone();

        let new_generics = |is_own| {
            let mut gen = my_e_cxt.frame_generics(is_own).clone();

            let e_id = my_e_cxt.id();
            let lib_path = gen::lib_path();
            gen.params.push(syn::parse_quote!(#zip_param));
            let wc = gen.where_clause.get_or_insert_with(|| syn::WhereClause {
                where_token: syn::token::Where::default(),
                predicates: syn::punctuated::Punctuated::new(),
            });

            let zipper_trait = gen::trai::lib::zipper();
            let (_, params, _) = my_e_cxt.generics().split_for_impl();
            wc.predicates
                .push(syn::parse_quote!(#zip_param: #lib_path :: #zipper_trait<#e_id #params>));

            gen
        };

        let mut stacks = Map::new();
        for other_e_idx in my_e_cxt.fp_e_deps().iter().cloned() {
            let prev = stacks.insert(other_e_idx, ZipField::new(&cxt[other_e_idx]));
            debug_assert_eq!(prev, None);
        }

        Self {
            id,
            own_generics: new_generics(true),
            ref_generics: new_generics(false),

            zipper_field_id,
            zipper_field_typ,

            stacks,
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
}

impl ZipStruct {
    pub fn to_struct_def_tokens(&self, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let generics = self.generics(is_own);
        let (params, _, where_clause) = generics.split_for_impl();

        let zipper_id = &self.zipper_field_id;
        let zipper_typ = &self.zipper_field_typ;

        let stacks = self
            .stacks
            .values()
            .map(|stack| stack.to_field_tokens(is_own));

        quote! {
            pub struct #id #params #where_clause {
                pub #zipper_id: #zipper_typ,
                #(#stacks ,)*
            }
        }
    }
}
