open Asttypes
open Config
open Format
open Typed_ast
open Typed_ast_printer

open Aez
open Smt

let tofi i = Term.make_int (Num.Int i)
let tplus a b = Term.make_arith Term.Plus a b
let one = tofi 1
let zero = tofi 0
let empty_f = Formula.f_true
let fgen_verbose = ref false

(** Custom print function for formulae *)

let rec pp_formula fmt phi = 
    match phi with
      | Formula.Lit a -> Literal.LT.print fmt a
      | Formula.Comb (Formula.Not, [f]) -> 
	  fprintf fmt "%s%snot%s(%a)" cbold cdarkgray cdef pp_formula f
      | Formula.Comb (Formula.And, l) -> fprintf fmt "@[(%a)@]" (print_list "and") l
      | Formula.Comb (Formula.Or, l) ->  fprintf fmt "@[(%a)@]" (print_list "or") l
      | Formula.Comb (Formula.Imp, [f1; f2]) -> 
	  fprintf fmt "@[<1>(%a %s%s=>%s %a)@]"
		pp_formula f1 cbold cblue cdef pp_formula f2
      | _ -> assert false
  and print_list sep fmt = function
    | [] -> ()
    | [f] -> pp_formula fmt f
    | f::l -> 
	  fprintf fmt "%a @ %s%s%s%s %a"
		pp_formula f cbold cblue sep cdef (print_list sep) l

let pp_term fmt term =
  ()

let rec pp_term_list fmt tl =
  match tl with
  | [] -> print_endline "End list"
  | hd :: tl ->
	pp_term fmt tl; print_string "-"; pp_term_list fmt tl

let rec p_il fmt = function
	| [] -> ()
	| [i] -> print_int i
	| i::tl -> (print_int i);(p_il fmt tl)

(** Errors *)
type fgen_error =
| NotNormalizedForm of Typed_ast.t_expr
| UnrevertedComparison of Asttypes.op
| UnexpectedExpression
| UnexpectedTerm of string * Term.t
| UnexpectedFormula of Formula.t

exception FgenError of fgen_error

let fgen_report fmt = function
  | NotNormalizedForm e -> 
	fprintf fmt "Expression not in normalized form : %a@."
	  print_exp e;
	begin
	match e.texpr_desc with
	| TE_app (_,_) -> fprintf fmt "It's an application !@."
	| TE_tuple _ -> fprintf fmt "It's a tuple !@."
	| _ -> fprintf fmt "And I don't know what it is.."
	end;
  | UnrevertedComparison op -> 
	fprintf fmt "You should revert comparisons : %a@."
	print_op op
  | UnexpectedExpression ->
	fprintf fmt "Unexpected expression@."
  | UnexpectedFormula f -> 
	fprintf fmt "Unexpected formula, awaiting a term : %a@."
	  Formula.print f
  | UnexpectedTerm (s, t) ->
	fprintf fmt "Unexpected term, awaiting a formula %s" s

let fgen_error e = raise (FgenError(e))

(** Translate from lustre to smt *)
type formula_operator =
| FormOpComb of Formula.combinator
| FormOpComp of Formula.comparator
| FormOpArith of Term.operator
| FormIte of (Formula.t -> Term.t -> Term.t -> Term.t)

type exp_translation =
| ExpFormula of Formula.t
| ExpTerm of Term.t

(* Before translation we need to revert gt and ge *)
let revert_comp e =
  match e.texpr_desc with
  | TE_op (op, el) ->
	let rev_op, revert =
	  match op with
	  | Op_gt -> Op_lt , true
	  | Op_ge -> Op_le , true
	  | _ -> op , false
	in
	let elist = if revert then List.rev el else el in
	let	ndesc = TE_op (rev_op, elist) in
	{e with texpr_desc = ndesc}
  | _ -> e
  
(* Types of expressions must not be tuples *)
let type_of_expr = 
  fun e -> match e.texpr_type with [t] -> t | _ -> fgen_error (NotNormalizedForm e)


let translate_ty ast_type =
  match ast_type with
  | Tbool -> Type.type_bool
  | Tint -> Type.type_int
  | Treal -> Type.type_real

let translate_op op =
  match op with
  | Op_eq -> FormOpComp Formula.Eq
  | Op_neq -> FormOpComp Formula.Neq
  | Op_lt -> FormOpComp Formula.Lt
  | Op_le -> FormOpComp Formula.Le
  | Op_ge | Op_gt -> fgen_error (UnrevertedComparison op)
  | Op_and -> FormOpComb Formula.And
  | Op_or -> FormOpComb Formula.Or
  | Op_not -> FormOpComb Formula.Not
  | Op_impl -> FormOpComb Formula.Imp
  | Op_add | Op_add_f -> FormOpArith Term.Plus
  | Op_sub | Op_sub_f -> FormOpArith Term.Minus
  | Op_mul | Op_mul_f -> FormOpArith Term.Mult
  | Op_div | Op_div_f -> FormOpArith Term.Div
  | Op_mod -> FormOpArith Term.Modulo
  | Op_if -> FormIte Term.make_ite


let extract_formula indice = function
  | ExpFormula f -> f
  | ExpTerm t -> fgen_error (UnexpectedTerm(indice, t))

let extract_term = function
  | ExpTerm t -> t
  | ExpFormula f -> fgen_error (UnexpectedFormula f)

let to_formula = function
  | ExpTerm t -> Formula.make_lit Formula.Eq [t; Term.t_true]
  | ExpFormula f -> f

let count_types li =
  let rec ct li i j =
	match li with
	| [] -> (i,j)
	| hd :: tl ->
	  begin
		match hd with
		| ExpTerm _ -> ct tl (i+1) j
		| ExpFormula _ -> ct tl i (j+1)
	  end
  in
  ct li 0 0

let print_li_type_count fmt li =
  let (i,j) = count_types li in
  Format.fprintf fmt "@. %i terms and %i formulas" i j

(** Declare symbols and build maps from idents to symbols *)

module VarToSymbols = Map.Make(Ident)

let declare_symbol name t_in t_out =
  let x = Hstring.make name in
  Symbol.declare x t_in t_out ;
  x


let rec declare_symbols_in_map map var_list =
  match var_list with
  |[] -> map
  | hd :: tl ->
	let id = fst hd in
	let id_type = snd hd in
	let new_map = VarToSymbols.add id
	  (declare_symbol (Ident.string_of id) [Type.type_int] (translate_ty id_type))
	  map 
	in declare_symbols_in_map new_map tl

let declare_symbols var_list =
  declare_symbols_in_map VarToSymbols.empty var_list

let find_id id map =
  try VarToSymbols.find id map with Not_found -> failwith "Ident not found in map"

let cond_append elt elt_list = 
  if List.mem elt elt_list then elt_list else elt::elt_list

let incr_part f = fun n -> snd ((fst f) n)
let ok_part f = snd f
let state_part f = fun n -> fst ((fst f) n)
