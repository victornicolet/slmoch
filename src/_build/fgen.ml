open Asttypes
open Typed_ast
open Aez
open Smt
open Smt_utils
open Set

let term_zero = Term.make_int (Num.Int 0)
let term_one = Term.make_int (Num.Int 1)

let state_vars = Stack.create ()

let rec gen_term_of_exp n ids exp =
  match exp.texpr_desc with
  | TE_const c -> 
	begin
	  match c with
	  | Cbool b -> ExpTerm (if b then Term.t_true else Term.t_false)
	  | Cint i -> ExpTerm (Term.make_int (Num.Int i))
	  | Creal r -> failwith "FUCK THE REAL NUMBERS"
	end
  | TE_ident id -> ExpTerm 
	(Term.make_app (find_id id ids) [n])
  | TE_op (op, el) -> 
	let term_list = List.map (gen_term_of_exp n ids) el in
	let wrapped_op = translate_op op in
	begin
	  match wrapped_op with 
	  | FormOpComp comp -> 
		ExpFormula (Formula.make_lit comp
					  (List.map extract_term term_list))
	  | FormOpComb comb ->
		ExpFormula (Formula.make comb
					  (List.map to_formula term_list))
	  | FormOpArith op ->
		assert(List.length term_list = 2);
		ExpTerm (Term.make_arith 
				   op
				   (extract_term (List.nth term_list 0))
				   (extract_term (List.nth term_list 1))
		)
	  | FormIte itemaker ->
		assert(List.length term_list = 3);
		ExpTerm (
		  itemaker  
			(to_formula (List.nth term_list 0))
			(extract_term (List.nth term_list 1))
			(extract_term (List.nth term_list 2))
		)
	end
  | TE_pre (expr) ->
	let pre_exp = 
	  gen_term_of_exp (Term.make_arith Term.Minus n term_one) ids expr
	in
	Stack.push (extract_term pre_exp) state_vars;
	pre_exp
  | TE_arrow (e1, e2) -> 
	let f1 = gen_term_of_exp n ids e1 in
	let f2 = gen_term_of_exp n ids e2 in
	let term = Term.make_ite
	  (Formula.make_lit Formula.Eq 
		 [n; term_zero])
	  (extract_term f1) (extract_term f2)
	in ExpTerm(term)
  | TE_prim (f, el) ->
	let term_list = List.map (gen_term_of_exp n ids) el in
	let input_types = 
	  List.map translate_ty 
		(List.map type_of_expr el)
	in 
	let output_type = translate_ty (type_of_expr exp) in
	let fsymb = declare_symbol (Ident.string_of f) input_types output_type in
	ExpTerm (Term.make_app fsymb (List.map extract_term term_list))
  | _ -> fgen_error (NotNormalizedForm(exp))


let gen_formula_of_eqn n v_to_s {teq_patt = tp; teq_expr = te} =
  (* equations are in normal form, no tuples *)
  assert(List.length tp.tpatt_desc = 1);
  let rf = gen_term_of_exp n v_to_s te in
  let x = Term.make_app 
	(find_id (List.nth tp.tpatt_desc 0) v_to_s) [n] 
  in
  match rf with 
  | ExpTerm t -> 
	Formula.make_lit Formula.Eq [x; (extract_term rf)]
  | ExpFormula f ->
	let fx = Formula.make_lit Formula.Eq [x; Term.t_true] in
	Formula.make Formula.And
	  [
		Formula.make Formula.Imp [fx; f];
		Formula.make Formula.Imp [f; fx]
	  ]

let gen_formula_node node =
  (* Build a map of the node variables to the symbols of the formulae *)
  let symbol_map = 
	declare_symbols ((node.tn_input)@(node.tn_output)@(node.tn_local))
  in
  (* Définition du noeud *)
  let rec aux_el v_to_s eqn_list n =
	match eqn_list with
	| [] ->  []
	| hd :: tl ->
	  let hd_f = gen_formula_of_eqn n v_to_s hd in
	   (hd_f) :: (aux_el v_to_s tl n) 
  in
  (* Invariant à prouver *)
  let def_ok n = 
	Formula.make_lit Formula.Eq
	  [
		Term.make_app (find_id (fst (List.nth node.tn_output 0)) symbol_map) 
		  [n];
		Term.t_true
	  ]
  in
  (fun n -> Formula.make Formula.And (aux_el symbol_map node.tn_equs n)),
  (fun n -> def_ok n)
