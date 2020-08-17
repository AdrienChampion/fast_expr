//! Operations over types.

prelude! {}

use rust::*;

// /// Retrieves the expression index and optional type parameters of a type.
// pub fn to_expr(cxt: &cxt::Cxt, typ: &Typ) -> Option<(idx::Expr, Option<TypParams>)> {
//     let path = match typ {
//         Typ::Path(path) if path.qself.is_none() => &path.path,
//         _ => return None,
//     };

//     let mut path = path.segments.iter();

//     let typ = match path.next() {
//         Some(segment) if segment.ident == "self" && segment.arguments.is_empty() => {
//             if let Some(segment) = path.next() {
//                 segment
//             } else {
//                 return None;
//             }
//         }
//         Some(segment) => segment,
//         None => return None,
//     };

//     let arguments = match &typ.arguments {
//         syn::PathArguments::None => None,
//         syn::PathArguments::AngleBracketed(args) => {

//         },
//         syn::PathArguments::Parenthesized(_) => return None,
//     }

//     cxt.sub_cxt_of(&typ.ident).map()

//     None
// }
