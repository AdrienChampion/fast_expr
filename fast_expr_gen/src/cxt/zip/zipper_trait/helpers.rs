//! Helper traits.

use super::*;

#[derive(Clone, Debug)]
pub struct Helpers {
    eval_sc: EvalSc,
}
impl Helpers {
    pub fn new(
        e_cxt: &cxt::frame::ECxt,
        v_handlers: &VariantHandlers,
        zipper: &ZipperTrait,
    ) -> Self {
        Self {
            eval_sc: EvalSc::new(e_cxt, v_handlers, zipper),
        }
    }

    pub fn to_tokens(&self, cxt: &cxt::ZipCxt, is_own: bool) -> TokenStream {
        self.eval_sc.to_decl_tokens(cxt, is_own)
    }
}

#[derive(Clone, Debug)]
pub struct EvalSc {
    id: Ident,
    e_idx: idx::Expr,
    own_generics: rust::Generics,
    ref_generics: rust::Generics,
    v_handlers: idx::VariantMap<(Ident, FnParams)>,
    c_handlers: (),
}
impl EvalSc {
    pub fn new(
        e_cxt: &cxt::frame::ECxt,
        v_handlers: &VariantHandlers,
        zipper: &ZipperTrait,
    ) -> Self {
        let e_idx = e_cxt.e_idx();
        let expr = e_cxt.expr();
        let e_id = expr.e_id();
        let id = gen::typ::eval_sc(e_id);

        let (own_generics, ref_generics) = (
            zipper.generics(true).clone(),
            zipper.generics(false).clone(),
        );

        let v_handlers = v_handlers
            .iter()
            .map(|handler| {
                let v_idx = handler.v_idx;
                let id = gen::fun::eval_sc(e_id, expr[v_idx].v_id());
                let params = handler.go_up_params.clone();
                (id, params)
            })
            .collect();

        Self {
            id,
            e_idx,
            own_generics,
            ref_generics,
            v_handlers,
            c_handlers: (),
        }
    }

    pub fn to_assoc_typ_tokens(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        let expr_res = cxt[self.e_idx].res_typ();
        quote! { type #expr_res }
    }

    pub fn to_fun_tokens<'me>(
        &'me self,
        cxt: &'me cxt::ZipCxt,
        is_own: IsOwn,
    ) -> impl Iterator<Item = TokenStream> + 'me {
        let expr_res = cxt[self.e_idx].res_typ();

        self.v_handlers.iter().map(move |(id, params)| {
            let params = params.iter().map(|param| {
                let id = param.id();
                let typ = param.typ_with_forced_many_acc(cxt, is_own, || expr_res.clone());
                quote! { #id: #typ }
            });
            quote! {
                fn #id ( #(#params),* ) -> Self::#expr_res
            }
        })
    }

    pub fn to_decl_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let id = &self.id;
        let (t_params, _, _) = if is_own {
            self.own_generics.split_for_impl()
        } else {
            self.ref_generics.split_for_impl()
        };

        let fp_e_deps = cxt[self.e_idx].fp_e_deps();

        let content = fp_e_deps.iter().cloned().map(|e_idx| {
            let expr_res = cxt[e_idx].res_typ();
            let v_handler = cxt[e_idx].helpers().eval_sc.to_fun_tokens(cxt, is_own);
            quote! {
                type #expr_res;

                #(#v_handler ;)*
            }
        });

        quote! {
            pub trait #id #t_params {
                #(#content)*
            }
        }
    }
}
