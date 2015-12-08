open Aez
open Config
open Smt
open Smt_utils

module BMC_solver = Smt.Make(struct end)
module IND_solver = Smt.Make(struct end)

let path_compression = ref true

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

let solve_k n acc (delta_incr, ok) k =
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
  let hstrp = (fun s fmt () -> Aez.Hstring.print fmt s) in
  try
	begin
	  match solve_k n acc f k with
	  | false, _ -> Format.fprintf Config.formatter "@.DONE @."
	  | true, entailment_acc -> kindly n entailment_acc f (k+1)
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

(** Main *)
let this f =
    let n = Term.make_app (declare_symbol "n" [] Type.type_int) [] in
	init_ind_case n (fst f);
	kindly n empty_f f 0
