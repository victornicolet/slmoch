open Typed_ast
open Smt_utils
open Asttypes

module NodeMap = Map.Make(Ident)


(** new_local génère une nouvelle varaible locale à chaque appel de la fonction *)
let new_local =
  let cpt = ref 0 in fun () -> incr cpt; Ident.make ("aux_"^(string_of_int !cpt)) Ident.Stream

let new_pat ({ texpr_type= ty; texpr_loc = loc } as e) =
  match ty with
  | [t] -> 
	let x = new_local () in
	let decl = [x, t] in
	let patt = { tpatt_desc = [x]; tpatt_type = ty; tpatt_loc = loc } in
	let expr = { e with texpr_desc = (TE_ident x) } in
	decl, patt, expr
  | lt ->
	let lx = List.map (fun _  -> new_local () ) lt in
	let decl = List.combine lx lt in
	let patt ={tpatt_desc = lx; tpatt_type = ty; tpatt_loc = loc } in
	let le = 
	  List.map 
		(fun (x,t) ->
		  {texpr_desc = TE_ident x; texpr_type = [t]; texpr_loc = loc}
		) decl
	in
	decl, patt, { e with texpr_desc = TE_tuple le}

(** Tuples *)

let is_tuple_eqn eqn =
  List.length eqn.teq_patt.tpatt_desc > 1

let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
	(x :: List.map List.hd xss ) :: 
	  transpose (xs :: List.map List.tl xss)

let detuple tuple = 
  match tuple.texpr_desc with 
  | TE_tuple (el) -> el
  | _ -> failwith "Not a tuple"

let rec desingle x =
  match x.texpr_desc with
  | TE_tuple([e]) -> e
  | _ -> x

let remap_with eqn_expr f =
  (fun el ty ->
		{ texpr_desc = f el ;
		  texpr_type = [ty];
		  texpr_loc = eqn_expr.texpr_loc } 
   )

let tuple_expr el types loc =
  	{ texpr_desc = TE_tuple (el) ;
	  texpr_type = types;
	  texpr_loc = loc }

let rec lift_tuples types_list eqn_expr =
  match eqn_expr.texpr_desc with
  | TE_op (op, expr_list) ->
	let lifted = List.map (lift_tuples types_list) expr_list in
	(* expr list should be a list of tuples *)
	let expr_list_no_tuple = List.map detuple lifted in
	let transposed = transpose expr_list_no_tuple in
	let remapped = List.map2
	  (remap_with eqn_expr (fun el -> TE_op(op, el)))
	  transposed types_list in
	tuple_expr remapped types_list eqn_expr.texpr_loc

  | TE_arrow (e1, e2) ->
	let li = transpose [detuple (lift_tuples types_list e1);
	  detuple (lift_tuples types_list e2)] in
	let remapped = List.map2
	  (remap_with eqn_expr (fun el -> TE_arrow (List.nth el 0, List.nth el 1)))
	  li types_list
	in
    tuple_expr remapped types_list eqn_expr.texpr_loc
  | TE_pre e ->
	let e_tup = lift_tuples types_list e in
	let e' = detuple e_tup in
	let remapped = List.map2
	  (remap_with eqn_expr (fun e -> TE_pre (e)))
	  e' types_list in
	tuple_expr remapped types_list eqn_expr.texpr_loc
  | TE_tuple (expr_list) ->
	begin
	  match expr_list with
	  |[e] -> e
	  | _ -> eqn_expr
	end
  | _ -> eqn_expr

let rec break_tuples eqn =
  let loc = eqn.teq_patt.tpatt_loc in
  let extract_idents_types = List.map2
	(fun i t -> (i,t)) eqn.teq_patt.tpatt_desc eqn.teq_patt.tpatt_type
  in 
  let extract_expressions = detuple eqn.teq_expr in
  List.map2
	(fun (i,t) expr ->
	  let eq_patt = {tpatt_desc = [i]; tpatt_type = [t]; tpatt_loc = loc} in
	  {teq_patt = eq_patt; teq_expr = expr}
	)
	extract_idents_types extract_expressions

(** Inlining *)

let rec eq_list loc left right =
  match left, right with
  | [], [] -> []
  | hd_l :: tl_l, hd_r :: tl_r ->
	let patt = {
	  tpatt_desc = [(fst hd_l)];
	  tpatt_type = [Tbool];
	  tpatt_loc = loc;
	} in
	{ teq_patt = patt; teq_expr = hd_r}::(eq_list loc tl_l tl_r)
  | _, _ -> failwith "Number of arguments not matching number of input in node"


(** Normalization functions *)

let rec inline_func nodes ctx to_inline args =
  (* The variables of the node body are already unique, no need to rename them
	 when inserting the node body into the caller *)
  (* Normalize the node *)
  let n =
		List.fold_left 
		  (fun node e ->
			normalize_equation nodes node e)
		  {to_inline with tn_equs=[]}
		  to_inline.tn_equs
  in
  let to_inline = { n with tn_equs = List.rev n.tn_equs } in
  (* Add equations : arg = input values of function *)
  let assign_args = eq_list to_inline.tn_loc to_inline.tn_input args in
  let new_locals = 
	(to_inline.tn_local)@(to_inline.tn_output)@(to_inline.tn_input) in
  let output_expr = List.map
	(fun x -> {
	  texpr_desc = TE_ident (fst x);
	  texpr_type = [snd x];
		texpr_loc = to_inline.tn_loc
	}) to_inline.tn_output in
  let tuple_output = TE_tuple (output_expr) in
  (new_locals, assign_args@to_inline.tn_equs) , tuple_output

and detuplify eqn_list =
  match eqn_list with
  | [] -> []
  | hd :: tl  ->
	begin
	  let nhd = 
		if is_tuple_eqn hd then 
		  let tuple_eqn =
			{hd with teq_expr = lift_tuples hd.teq_patt.tpatt_type hd.teq_expr}
		  in
		  break_tuples tuple_eqn
		else 
		  [{hd with teq_expr = desingle hd.teq_expr}]
	  in
	  nhd@(detuplify tl)
	end

and normalize nodes ctx exp = 
  let e = Smt_utils.revert_comp exp in
  match e.texpr_desc with
  | TE_const _ | TE_ident _ -> ctx, e

  | TE_op (op, expr_list) ->
	let (new_vars, new_eqs), expr_list' = normalize_list nodes ctx expr_list in
	let x_decl, x_patt, x_expr = new_pat e in
	let x_eq = {
	  teq_patt = x_patt;
	  teq_expr = {e with texpr_desc = TE_op(op, expr_list')}
	} in
	(x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_app (f, expr_list) ->
	let (new_vars, new_eqs), expr_list' = normalize_list nodes ctx expr_list in
	let x_decl, x_patt, x_expr = new_pat e in
	let (vars_inline, eqs_inline), tuple =
	  inline_func
		nodes
		ctx
		(NodeMap.find f nodes)
		expr_list'
	in
	let x_eq = {
	  teq_patt = x_patt;
	  teq_expr = {e with texpr_desc = tuple }
	} in
	(x_decl@new_vars@vars_inline, x_eq::new_eqs@eqs_inline), x_expr

(* Par la suite, on se débarassera des tuples après avoir inliné toutes les fonctions
   or on ne peut pas inliner les primitives, on s'assure donc qu'elles nerenvoient pas de 
   tuples *)
  | TE_prim (f, expr_list) ->
	let (new_vars, new_eqs), expr_list' = normalize_list nodes ctx expr_list in
	let x_decl, x_patt, x_expr = new_pat e in
	assert(List.length x_patt.tpatt_type < 2);
	let x_eq = {
	  teq_patt = x_patt;
	  teq_expr = {e with texpr_desc = TE_prim(f, expr_list')}
	} in
	(x_decl@new_vars, x_eq::new_eqs), x_expr
   
  | TE_arrow (e1, e2) ->
	let ctx, e1' = normalize nodes ctx e1 in
	let ctx, e2' = normalize nodes ctx e2 in
	ctx, { e with texpr_desc = TE_arrow (e1', e2')}
	  
  | TE_pre e ->
	let ctx, e' = normalize nodes ctx e in
	ctx, {e with texpr_desc = TE_pre e'}

  | TE_tuple tex_l -> 
	let ctx, l' = normalize_list nodes ctx tex_l in
	ctx, {e with texpr_desc = TE_tuple l'}

and normalize_list nodes ctx el =
  let ctx, list =
	List.fold_left
	  (fun (ctx, l) e -> 
		let ctx, e' = normalize nodes ctx e in
		ctx, e'::l) (ctx,[]) el
  in ctx, List.rev list

and normalize_equation nodes node e =
  let (locals, new_eqs), e' = normalize nodes ([],[]) e.teq_expr in
  { node with
    tn_local = locals@node.tn_local;
    tn_equs = { e with teq_expr = e' } :: 
	  (List.rev (detuplify new_eqs)) @ (detuplify node.tn_equs) }

let file node_list =
  let node_map = List.fold_left 
	(fun map n -> NodeMap.add n.tn_name n map)
	NodeMap.empty
	node_list
  in
  List.map
    (fun n ->
      let n =
		List.fold_left 
		  (fun n e -> normalize_equation node_map n e)
		  { n with tn_equs=[] }
		  n.tn_equs
      in
      { n with tn_equs = List.rev n.tn_equs })
	node_list
