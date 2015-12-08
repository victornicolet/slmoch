open Asttypes

type node = aez_formula list

and file = node list

and aez_formula =
| Term of aez_term
| Comp of aez_comparator * aez_term * aez_term
| Comb of aez_comb * aez_term * aez_term

and aez_term =
| Constant of Asttypes.const
| ArithOp of aez_op * aez_term * aez_term
| Ite of aez_formula * aez_term * aez_term
| App of string * (aez_term list)

and aez_comb =
| And | Or | Impl | Not

and aez_op =
| Plus | Minus | Times | Slash | Mod

and aez_comparator =
| Eq | Neq | Le | Leq
 
