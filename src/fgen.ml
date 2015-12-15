open Asttypes
open Typed_ast
open Aez
open Smt
open Smt_utils
open Set



let rec gen_term_of_exp n state_vars ids exp =
  match exp.texpr_desc with
  | TE_const c -> 
	let term =
	  match c with
	  | Cbool b -> ExpTerm (if b then Term.t_true else Term.t_false)
	  | Cint i -> ExpTerm (Term.make_int (Num.Int i))
	  | Creal r -> failwith "Real numbers unsupported"
	in
	state_vars, term

  | TE_ident id ->
	state_vars, ExpTerm (Term.make_app (find_id id ids) [n])

  | TE_op (op, el) -> 
	let sv, term_list = gen_term_list_of_el n state_vars ids el in
	let wrapped_op = translate_op op in
	let ntl =
	  match wrapped_op with 
	  | FormOpComp comp -> 
		ExpFormula (Formula.make_lit comp
					  (List.map extract_term term_list))
	  | FormOpComb comb ->
		ExpFormula (Formula.make comb
					  (List.map to_formula term_list))
	  | FormOpArith op ->
		let n_args = List.length term_list in
		assert(n_args = 1 || n_args = 2);
		if n_args = 2 then
		  begin
			ExpTerm (Term.make_arith 
					   op
					   (extract_term (List.nth term_list 0))
					   (extract_term (List.nth term_list 1))
			)
		  end
		else
		  begin
			ExpTerm (Term.make_arith
					   op
					   Smt_utils.zero
					   (extract_term (List.nth term_list 0))
			)
		  end
		  
	  | FormIte itemaker ->
		assert(List.length term_list = 3);
		ExpTerm (
		  itemaker  
			(to_formula (List.nth term_list 2))
			(extract_term (List.nth term_list 0))
			(extract_term (List.nth term_list 1))
		)
	in sv, ntl

  | TE_pre (expr) ->
	let  sv, pre_exp = 
	  gen_term_of_exp
		(Term.make_arith Term.Minus n one) state_vars ids expr
	in
	(cond_append pre_exp sv), pre_exp

  | TE_arrow (e1, e2) -> 
	let sv0, f1 = gen_term_of_exp n state_vars ids e1 in
	let sv, f2 = gen_term_of_exp n sv0 ids e2 in
	let term = Term.make_ite
	  (Formula.make_lit Formula.Eq 
		 [n; zero])
	  (extract_term f1) (extract_term f2)
	in sv,  ExpTerm(term)

  | TE_prim (f, el) ->
	let sv, term_list = gen_term_list_of_el n state_vars ids el  in
	let input_types = 
	  List.map translate_ty 
		(List.map type_of_expr el)
	in 
	let output_type = translate_ty (type_of_expr exp) in
	let fsymb = declare_symbol (Ident.string_of f) input_types output_type in
	sv, ExpTerm (Term.make_app fsymb (List.map extract_term term_list))

  | _ -> fgen_error (NotNormalizedForm(exp))


and gen_term_list_of_el n sv ids el =
  let rec aux sv el te_l =
	match el with
	| [] -> sv, te_l
	| hd :: tl -> 
	  let nsv, t = gen_term_of_exp n sv ids hd in
	   aux sv tl  (t::te_l)
  in
  aux sv el []

let gen_formula_of_eqn n state_vars v_to_s {teq_patt = tp; teq_expr = te}  =
  (* equations are in normal form, no tuples *)
  assert(List.length tp.tpatt_desc = 1);
  let sv, rf = gen_term_of_exp n state_vars v_to_s te in
  let x = Term.make_app 
	(find_id (List.nth tp.tpatt_desc 0) v_to_s) [n] 
  in
  let formula =
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
  in
  sv, formula

let gen_formula_node node =
  (* Build a map of the node variables to the symbols of the formulae *)
  let symbol_map = 
	declare_symbols ((node.tn_input)@(node.tn_output)@(node.tn_local))
  in
  (* Définition du noeud *)
  let rec aux_el state_vars  v_to_s eqn_list n =
	match eqn_list with
	| [] ->  [], []
	| hd :: tl ->
	  let sv, formulas = aux_el state_vars v_to_s tl n in
	  let nsv, hd_f = gen_formula_of_eqn n  sv v_to_s hd in
	  nsv,  (hd_f) :: formulas
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
  (fun n -> 
	let state_variables, formulas = aux_el [] symbol_map node.tn_equs n in
	state_variables, Formula.make Formula.And formulas 
  ),
  (fun n -> def_ok n)
