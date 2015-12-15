(* Programme principal *)
open Aez
open Config
open Fgen
open Format
open Ident
open Lexing
open Lexer
open Normalization
open Parser
open Parse_ast
open Smt
open Smt_utils
open Solve
open Typed_ast



let usage = "usage: "^Sys.argv.(0)^" [options] file.lus main"

let parse_only = ref false
let type_only = ref false
let norm_only = ref false
let lucy_printer = ref false
let ocaml_printer = ref true
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only", Arg.Set type_only, "  stops after typing";
   "-norm-only", Arg.Set norm_only, "  stops after normalization";
   "-verbose", Arg.Set verbose, "print intermediate transformations";
   "-verbose-solve", Arg.Set Solve.verbose, "verbose solve";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let file, main_node =
  let file = ref None in
  let main = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let set_main s =
    main := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | 2 -> set_main s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1),
  (match !main with Some n -> n | None -> Arg.usage spec usage; exit 1)

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let ft = Typing.type_file f main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Typed ast                          */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    if !type_only then exit 0;

	let fn = Normalization.file ft in
	if !verbose then begin
	  Format.printf "/**************************************/@.";
	  Format.printf "/* Normalized ast                     */@.";
	  Format.printf "/**************************************/@.";
	  Typed_ast_printer.print_node_list_std fn
	end; 
	if !norm_only then exit 0;
	if main_node = "" then exit 0;
	let mnode = List.find 
	  (fun node -> (node.tn_name.name = main_node)) fn 
	in
	let formula = Fgen.gen_formula_node mnode in
	if !verbose then begin
	  Format.printf "/**************************************/@.";
	  Format.printf "/* Formula                            */@.";
	  Format.printf "/**************************************/@.";
	  let m = Term.make_app (declare_symbol "m" [] Type.type_int) [] in
	  Format.fprintf 
		Config.formatter "@.Delta_incr :@.%a@.Invariant:@.%a@.@." 
		Smt_utils.pp_formula
		((incr_part formula) m) Formula.print ((ok_part formula) m);
	  Format.fprintf
		Config.formatter "@.%i State variables :@.%a@."
		(List.length ((state_part formula) m))
		Smt_utils.pp_term_list ((state_part formula) m);
	  Format.printf "@./**************************************/@.";
	  Format.printf "/* Checking...                          */@.";
	  Format.printf "/****************************************/@.";
	end;

	Solve.this formula;
    exit 0
  with
    | Lexical_error s ->
	  report_loc (lexeme_start_p lb, lexeme_end_p lb);
	  eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	  report_loc (lexeme_start_p lb, lexeme_end_p lb);
	  eprintf "syntax error\n@.";
	exit 1
    | Typing.Error(l,e) ->
	report_loc l;
	  eprintf "%a\n@." Typing.report e;
	exit 1
	| Smt_utils.FgenError(e) ->
	  eprintf "%a\n@." Smt_utils.fgen_report e;
    | e ->
        eprintf "Anomaly: %s\n@." (Printexc.to_string e);
        exit 2
