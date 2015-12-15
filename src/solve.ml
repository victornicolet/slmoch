open Aez
open Config
open Smt
open Smt_utils

module BMC_solver = Smt.Make(struct end)
module IND_solver = Smt.Make(struct end)
module LOOP_solver = Smt.Make(struct end)

let path_compression = ref true
let verbose = ref false

let base_case delta_incr ok k =
  let rec assume j =
	BMC_solver.assume ~id:k (delta_incr (tofi j));
	begin
	  if j > 0 then assume (j-1) else ()
	end
  in
  let rec entailment j acc =
	begin
	  if j > 0 then 
		entailment (j-1) ((ok (tofi j))::acc)
	  else
		(ok (tofi j))::acc
	end
  in
  BMC_solver.clear ();
  assume k;
  BMC_solver.check ();
  BMC_solver.entails ~id:0
	(Formula.make Formula.And (entailment k []))

let base_case_incr acc delta_incr ok k =
  let entailment = Formula.make Formula.And [acc; ok (tofi k)] in
  BMC_solver.assume ~id:0 (delta_incr (tofi k));
  BMC_solver.check();
  (BMC_solver.entails ~id:0 entailment, entailment)

let init_ind_case n delta_incr =
  IND_solver.assume ~id:0 (Formula.make_lit Formula.Le [zero; n]);
  IND_solver.assume ~id:0 (delta_incr n);
  IND_solver.check ()

let ind_case n delta_incr ok k =
  let kt = tofi k in
  let kt_p_k = tplus n kt in
  IND_solver.assume ~id:0 (delta_incr (tplus kt_p_k one));
  IND_solver.assume ~id:0 (ok kt_p_k);
  IND_solver.check ();
  IND_solver.entails ~id:0 (ok (tplus kt_p_k one))

(** LOOP **)
let rec make_loop_cond prev_states new_states =
  let rec join prev_states new_states =
	match prev_states with
	| [] -> []
	| hd::tl ->
      (formula_of_trl hd new_states)::(join tl new_states)
  in
 Formula.make Formula.Or (join prev_states new_states) 

let check_noloop prev_states states delta_incr k =
  let kt = tofi k in
  let states_at_k1 = states (tplus kt one) in
  let loop_condition = make_loop_cond prev_states states_at_k1 in
  (if !verbose then
	  Format.fprintf Config.formatter "LOOP CONDITION %i:@.%a@."
		k
		pp_formula loop_condition
  );
  let new_states = (states_at_k1)::prev_states in
  LOOP_solver.assume ~id:0 (delta_incr kt);
  (LOOP_solver.entails ~id:0 loop_condition), new_states

let solve_k n acc f k =
  let delta_incr = incr_part f in
  let ok = ok_part f in
  let base, entailments_acc = base_case_incr acc delta_incr ok k in
  let induction = ind_case n delta_incr ok k in
  let next_step = 
	match base, induction with
	| false, _ -> 
	  Format.fprintf Config.formatter "@.FALSE PROPERTY, k = %i@." k; 
	  false
	| true, false -> 
	  true
	| true, true ->
	  Format.fprintf Config.formatter "@.TRUE PROPERTY, k = %i@." k;
	  false
  in
  next_step, entailments_acc

let rec kindly n acc f k =
	if !verbose then
	  begin
	  print_string "k = "; print_int k; print_endline "..."
  end;
	let hstrp = (fun s fmt () -> Aez.Hstring.print fmt s) in
	if k <= !Config.kinduction_limit then
	  try
		begin
		  match solve_k n (fst acc) f k with
		  | false, _ -> Format.fprintf Config.formatter "@.DONE @."
		  | true, entailment_acc -> 
			let loopcheck, new_states = 
			  check_noloop (snd acc) (state_part f) (incr_part f) k
			in
			if loopcheck then
			  Format.fprintf Config.formatter 
				"@.LOOP AT %i STEP.@. PROPERTY HOLDS @."
				k
			else
			  kindly n (entailment_acc, new_states) f (k+1)
		end
	  with
	  | Smt.Error e -> 
		let str, pp = 
		  match e with
		  | Smt.DuplicateSymb s -> "Duplicate symbol ", hstrp s
		  | Smt.DuplicateTypeName s -> "Duplicate type name ", hstrp s
		  | Smt.UnknownSymb s-> "Unknown symbol ", hstrp s
		  | Smt.UnknownType s -> "Unknown type ", hstrp s
		in
		Format.fprintf formatter "%sError in solver :%s @.%s : %a@."
		  cred cdef str pp ()
	  | Smt.Unsat il ->
		Format.fprintf formatter "%sError in solver, unsat:%s@%a@."
		  cred cdef p_il il
	else
	  Format.fprintf Config.formatter "@.DON'T KNOW@."

(** Main *)
let this f =
    let n = Term.make_app (declare_symbol "n" [] Type.type_int) [] in
	let first_states = (state_part f)(tofi 0) in
	init_ind_case n (incr_part f);
	kindly n (empty_f,[first_states]) f 0
