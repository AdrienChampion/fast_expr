//! Pre-contexts, contexts used generated by the first compilation step.

prelude! {}

use cxt::CollCxt;

pub type ECxts = idx::ExprMap<ECxt>;

#[derive(Debug, Clone)]
pub struct ECxt {
    /// Expression index.
    e_idx: idx::Expr,

    /// Result type identifier.
    res_typ_id: rust::Id,
    /// Result type.
    res_typ: rust::Typ,

    /// Expression types mentioned by this expression's definition.
    ///
    /// Includes self if self-recursive.
    ///
    /// **Only valid after finalization.**
    e_deps: Set<idx::Expr>,

    /// Collection contexts.
    ///
    /// **Only valid after finalization.**
    colls: idx::CollMap<CollCxt>,

    /// Generics associated with this expression.
    generics: Option<rust::Generics>,

    /// Type parameters introduced by `self.generics()`.
    top_t_params: rust::GenericArgs,

    /// Frame-type identifier.
    frame_typ_id: rust::Id,
    /// Zip trait identifier.
    zip_trait_id: rust::Id,

    /// Expression definition from the frontend.
    def: rust::Enum,
}

impl ECxt {
    /// Constructor.
    pub fn new(cxt: &Cxt<Self>, e_idx: idx::Expr, def: rust::Enum) -> Res<Self> {
        let res_typ_id = gen::typ::res(&def.ident);
        let res_typ = rust::typ::plain(res_typ_id.clone(), None);

        let generics = {
            // macro_rules! spec_trait_list {
            //     ($str_pref:expr) => {{
            //         let specs = cxt.specs();
            //         if !specs.is_empty() {
            //             let mut blah = $str_pref.to_string();
            //             let mut pref = "";
            //             for id in cxt.specs().keys() {
            //                 blah.push_str(pref);
            //                 pref = ", ";
            //                 blah.push('`');
            //                 blah.push_str(&id.to_string());
            //                 blah.push('`');
            //             }
            //             blah
            //         } else {
            //             "".into()
            //         }
            //     }};
            // }

            let mut params = def.generics.params.iter();
            let first_param: Option<Option<&cxt::Spec>> = params.next().map(|param| match param {
                rust::GenericParam::Type(maybe_spec) => cxt.get_spec(&maybe_spec.ident),
                _ => None,
            });

            match (first_param, params.next()) {
                (Some(Some(spec)), None) => Some(spec.generics().clone()),
                (None, None) | (Some(None), _) => None,

                (Some(Some(_)), Some(param)) => bail!(on(
                    &param,
                    "expression types that use a specification-trait \
                    cannot have other type parameters"
                )),

                (None, Some(_)) => {
                    unreachable!("parameter iterator yielded `None`, and then `Some`")
                }
            }
        };

        let top_t_params = {
            let generics = generics.as_ref().unwrap_or(&def.generics);
            generics
                .params
                .iter()
                .map(|param| {
                    use rust::{GenericArg, GenericParam::*};
                    match param {
                        Type(typ) => {
                            let typ = &typ.ident;
                            GenericArg::Type(syn::parse_quote!(#typ))
                        }
                        Lifetime(lt) => GenericArg::Lifetime(lt.lifetime.clone()),
                        Const(cst) => {
                            let cst = &cst.ident;
                            GenericArg::Const(syn::parse_quote!(#cst))
                        }
                    }
                })
                .collect()
        };

        let e_deps = Set::new();

        let frame_typ_id = gen::frame::typ_id(&def.ident);
        let zip_trait_id = gen::trai::zipper(&def.ident);

        let colls = idx::CollMap::new();

        Ok(Self {
            e_idx,
            res_typ_id,
            res_typ,

            e_deps,
            colls,

            generics,

            top_t_params,

            frame_typ_id,
            zip_trait_id,

            def,
        })
    }

    pub fn generate_frame_info(
        cxt: &cxt::PreCxt,
        exprs: &idx::ExprMap<expr::Expr>,
    ) -> cxt::frame::Infos {
        debug_assert_eq!(cxt.e_cxts().len(), exprs.len());

        let mut res: idx::ExprMap<_> = cxt
            .e_cxts()
            .iter()
            .map(|e_cxt| cxt::frame::Info::new(e_cxt))
            .collect();

        // Used to compute the dependencies fixed-point.
        let mut known;
        let mut todo = vec![];

        for (e_idx, e_cxt) in cxt.e_cxts().index_iter() {
            res[e_idx].add_frame_params(e_idx, cxt.e_cxts());

            known = Set::new();
            debug_assert!(todo.is_empty());

            let fp_e_deps = res[e_idx].fp_e_deps_mut();
            debug_assert!(fp_e_deps.is_empty());

            let _is_new = fp_e_deps.insert(e_idx);
            debug_assert!(_is_new);
            let _is_new = known.insert(e_idx);
            debug_assert!(_is_new);

            todo.extend(e_cxt.e_deps.iter().cloned());

            'fp: loop {
                let dep_e_idx = if let Some(e_idx) = todo.pop() {
                    e_idx
                } else {
                    todo.clear();
                    break 'fp;
                };

                let fp_e_deps = &mut res[e_idx].fp_e_deps_mut();

                let _ = fp_e_deps.insert(dep_e_idx);
                let is_new = known.insert(dep_e_idx);
                if !is_new || dep_e_idx == e_idx {
                    continue 'fp;
                }

                // Alright, we're about to do some unsafe stuff. Nothing actually unsafe is going on
                // as long as THIS assert HOLDS.
                assert_ne!(dep_e_idx, e_idx);
                // Going to raw pointers here, because we need to borrow DIFFERENT CELLS of
                // `selves` as mutable and immutable at the same time.
                let fp_e_deps = {
                    let tmp: *mut _ = res[e_idx].fp_e_deps_mut();
                    unsafe { &mut *tmp }
                };
                let dep_fp_e_deps = {
                    let tmp: *const _ = res[dep_e_idx].fp_e_deps_mut();
                    unsafe { &*tmp }
                };

                // Have we already computed the fixed point of `dep`?
                if dep_e_idx < e_idx {
                    // Yeah, just merge it in `slf`.
                    known.extend(dep_fp_e_deps.iter().cloned());
                    fp_e_deps.extend(dep_fp_e_deps.iter().cloned());
                } else {
                    // No, add its dependencies as todos.
                    for sub_dep_e_idx in cxt[dep_e_idx].e_deps.iter().cloned() {
                        let is_new = fp_e_deps.insert(sub_dep_e_idx);
                        if is_new {
                            todo.push(sub_dep_e_idx)
                        }
                    }
                }
            }
        }

        debug_assert_eq!(cxt.e_cxts().len(), res.len());
        res
    }

    /// Index accessor.
    pub fn e_idx(&self) -> idx::Expr {
        self.e_idx
    }

    /// Plain type of the expression.
    pub fn plain_typ(&self) -> rust::Typ {
        let id = self.e_id();
        let (_, params, _) = self.generics().split_for_impl();
        syn::parse_quote!(#id #params)
    }
    /// Plain type under a reference if asked.
    pub fn plain_typ_for(&self, is_own: IsOwn) -> rust::Typ {
        let mut typ = self.plain_typ();
        if !is_own {
            typ = rust::typ::reference(Some(gen::lifetime::expr()), typ)
        }
        typ
    }

    /// Identifier accessor.
    pub fn e_id(&self) -> &rust::Id {
        &self.def.ident
    }
    /// Id of a variant.
    pub fn v_id(&self, v_idx: idx::Variant) -> &rust::Id {
        &self.def.variants[*v_idx].ident
    }

    pub fn e_deps(&self) -> &Set<idx::Expr> {
        &self.e_deps
    }

    /// Result type identifier.
    pub fn res_typ_id(&self) -> &rust::Id {
        &self.res_typ_id
    }
    /// Result type.
    pub fn res_typ(&self) -> &rust::Typ {
        &self.res_typ
    }
    /// Frame-type identifier.
    pub fn frame_typ_id(&self) -> &rust::Id {
        &self.frame_typ_id
    }
    /// Zip trait identifier.
    pub fn zip_trait_id(&self) -> &rust::Id {
        &self.zip_trait_id
    }

    /// Collection contexts accessor.
    pub fn colls(&self) -> &idx::CollMap<CollCxt> {
        &self.colls
    }
    pub fn push_coll(&mut self, coll: CollCxt) -> idx::Coll {
        self.colls.push(coll)
    }

    /// Expression enum definition.
    pub fn def(&self) -> &rust::Enum {
        &self.def
    }

    /// Generics accessor.
    pub fn generics(&self) -> &rust::Generics {
        self.generics.as_ref().unwrap_or(&self.def.generics)
    }
    /// Types introduced in the generics, as arguments.
    pub fn top_t_params(&self) -> &rust::GenericArgs {
        &self.top_t_params
    }

    pub fn log(&self, _pref: impl Display + Copy) {
        logln!(
            "{}expr({}) {}<{}>",
            _pref,
            self.e_idx(),
            self.e_id(),
            self.top_t_params
                .iter()
                .fold(String::new(), |acc, t| format!(
                    "{}{}{},",
                    acc,
                    if acc.is_empty() { "" } else { " " },
                    t.to_token_stream(),
                )),
        )
    }

    pub fn add_dep(&mut self, e_idx: idx::Expr) -> bool {
        self.e_deps.insert(e_idx)
    }
}
