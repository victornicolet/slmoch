type token =
  | AND
  | ARROW
  | BOOL
  | CONST
  | COLON
  | COMMA
  | COMP of (Asttypes.op)
  | CONST_BOOL of (bool)
  | CONST_INT of (int)
  | CONST_REAL of (float)
  | DIV
  | ELSE
  | END
  | EOF
  | EQUAL
  | NEQ
  | REAL
  | IDENT of (string)
  | IF
  | IMPL
  | INT
  | LET
  | LPAREN
  | MINUS
  | MOD
  | NODE
  | NOT
  | OR
  | PLUS
  | PRE
  | RETURNS
  | RPAREN
  | SEMICOL
  | SLASH
  | STAR
  | TEL
  | THEN
  | VAR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

  open Asttypes
  open Parse_ast

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }

# 53 "parser.ml"
let yytransl_const = [|
  257 (* AND *);
  258 (* ARROW *);
  259 (* BOOL *);
  260 (* CONST *);
  261 (* COLON *);
  262 (* COMMA *);
  267 (* DIV *);
  268 (* ELSE *);
  269 (* END *);
    0 (* EOF *);
  270 (* EQUAL *);
  271 (* NEQ *);
  272 (* REAL *);
  274 (* IF *);
  275 (* IMPL *);
  276 (* INT *);
  277 (* LET *);
  278 (* LPAREN *);
  279 (* MINUS *);
  280 (* MOD *);
  281 (* NODE *);
  282 (* NOT *);
  283 (* OR *);
  284 (* PLUS *);
  285 (* PRE *);
  286 (* RETURNS *);
  287 (* RPAREN *);
  288 (* SEMICOL *);
  289 (* SLASH *);
  290 (* STAR *);
  291 (* TEL *);
  292 (* THEN *);
  293 (* VAR *);
    0|]

let yytransl_block = [|
  263 (* COMP *);
  264 (* CONST_BOOL *);
  265 (* CONST_INT *);
  266 (* CONST_REAL *);
  273 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\006\000\
\006\000\009\000\009\000\010\000\010\000\011\000\007\000\007\000\
\014\000\015\000\015\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\017\000\017\000\017\000\012\000\012\000\018\000\018\000\
\019\000\019\000\013\000\013\000\013\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\015\000\000\000\001\000\001\000\000\000\
\002\000\001\000\003\000\002\000\003\000\003\000\001\000\002\000\
\004\000\001\000\005\000\003\000\001\000\001\000\004\000\006\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000\
\005\000\001\000\001\000\001\000\003\000\001\000\000\000\001\000\
\003\000\001\000\001\000\001\000\001\000\000\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\056\000\000\000\000\000\000\000\001\000\
\003\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\000\000\011\000\051\000\053\000\
\052\000\014\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\016\000\000\000\000\000\
\055\000\004\000\042\000\043\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\039\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\019\000\000\000\000\000\048\000\000\000\000\000\
\020\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\030\000\000\000\000\000\028\000\027\000\000\000\023\000\
\000\000\000\000\049\000\000\000\041\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\028\000\033\000\040\000\050\000\
\013\000\034\000\014\000\015\000\026\000\041\000\042\000\084\000\
\061\000\085\000\086\000"

let yysindex = "\004\000\
\245\254\000\000\000\255\000\000\020\000\245\254\021\255\000\000\
\000\000\028\255\042\255\020\255\000\000\023\255\047\255\028\255\
\026\255\028\255\255\254\000\000\031\255\000\000\000\000\000\000\
\000\000\000\000\028\255\029\255\000\000\025\255\036\255\028\255\
\054\255\000\000\044\255\022\255\028\255\000\000\062\255\046\255\
\022\255\074\255\000\000\077\255\060\255\000\000\194\000\028\255\
\000\000\000\000\000\000\000\000\000\000\076\255\194\000\194\000\
\194\000\194\000\194\000\056\000\000\000\073\255\194\000\063\255\
\028\000\069\255\000\000\000\000\194\000\194\000\194\000\194\000\
\194\000\194\000\194\000\194\000\194\000\194\000\194\000\000\000\
\194\000\194\000\000\000\080\000\078\255\000\000\194\000\194\000\
\000\000\173\000\128\000\061\255\000\000\061\255\061\255\143\000\
\069\255\000\000\158\000\069\255\000\000\000\000\194\000\000\000\
\104\000\079\255\000\000\194\000\000\000\128\000"

let yyrindex = "\000\000\
\107\000\000\000\000\000\000\000\000\000\107\000\000\000\000\000\
\000\000\081\255\002\255\000\000\000\000\084\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\087\255\000\000\
\000\000\000\000\000\000\000\000\095\255\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\088\255\000\000\
\000\000\099\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\089\255\000\000\000\000\000\000\000\000\
\000\000\230\255\207\255\168\255\000\000\191\255\214\255\005\000\
\122\255\000\000\228\255\145\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000"

let yygindex = "\000\000\
\000\000\115\000\000\000\000\000\000\000\000\000\084\000\000\000\
\242\255\095\000\240\255\246\255\000\000\000\000\000\000\209\255\
\000\000\000\000\171\255"

let yytablesize = 479
let yytable = "\060\000\
\054\000\023\000\106\000\022\000\001\000\020\000\046\000\064\000\
\065\000\066\000\067\000\068\000\029\000\003\000\024\000\035\000\
\007\000\107\000\025\000\008\000\035\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\046\000\101\000\102\000\022\000\022\000\062\000\038\000\105\000\
\022\000\022\000\010\000\039\000\011\000\022\000\022\000\016\000\
\022\000\022\000\017\000\019\000\027\000\022\000\018\000\021\000\
\031\000\022\000\022\000\030\000\110\000\022\000\022\000\069\000\
\070\000\022\000\022\000\022\000\022\000\071\000\022\000\072\000\
\032\000\072\000\036\000\037\000\073\000\074\000\044\000\072\000\
\045\000\075\000\048\000\076\000\077\000\076\000\077\000\047\000\
\079\000\078\000\079\000\049\000\077\000\081\000\082\000\081\000\
\082\000\063\000\087\000\038\000\038\000\081\000\082\000\083\000\
\038\000\038\000\002\000\008\000\104\000\109\000\038\000\005\000\
\038\000\038\000\010\000\012\000\015\000\038\000\047\000\050\000\
\009\000\038\000\026\000\026\000\046\000\038\000\038\000\026\000\
\026\000\038\000\038\000\043\000\000\000\026\000\038\000\026\000\
\026\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\026\000\025\000\025\000\000\000\026\000\026\000\025\000\025\000\
\026\000\026\000\000\000\000\000\025\000\026\000\025\000\025\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\025\000\
\031\000\031\000\000\000\025\000\025\000\031\000\031\000\025\000\
\025\000\000\000\000\000\031\000\025\000\031\000\031\000\000\000\
\000\000\000\000\031\000\000\000\000\000\000\000\000\000\032\000\
\032\000\000\000\031\000\000\000\032\000\032\000\031\000\031\000\
\000\000\000\000\032\000\031\000\032\000\032\000\000\000\000\000\
\000\000\032\000\000\000\000\000\037\000\000\000\033\000\033\000\
\000\000\032\000\037\000\033\000\033\000\032\000\032\000\000\000\
\000\000\033\000\032\000\033\000\033\000\035\000\034\000\034\000\
\033\000\035\000\000\000\034\000\000\000\037\000\037\000\035\000\
\033\000\034\000\037\000\000\000\033\000\033\000\035\000\000\000\
\034\000\033\000\000\000\000\000\000\000\000\000\035\000\000\000\
\034\000\000\000\035\000\035\000\034\000\034\000\036\000\035\000\
\000\000\034\000\036\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\024\000\000\000\000\000\000\000\000\000\036\000\
\024\000\054\000\000\000\000\000\069\000\070\000\000\000\000\000\
\000\000\088\000\071\000\036\000\036\000\000\000\072\000\000\000\
\036\000\073\000\074\000\024\000\024\000\000\000\075\000\000\000\
\024\000\000\000\076\000\077\000\000\000\000\000\078\000\079\000\
\069\000\070\000\089\000\000\000\081\000\082\000\071\000\000\000\
\000\000\000\000\072\000\000\000\000\000\073\000\074\000\000\000\
\000\000\000\000\075\000\000\000\000\000\000\000\076\000\077\000\
\069\000\070\000\078\000\079\000\000\000\103\000\071\000\080\000\
\081\000\082\000\072\000\000\000\000\000\073\000\074\000\000\000\
\000\000\000\000\075\000\000\000\000\000\000\000\076\000\077\000\
\069\000\070\000\078\000\079\000\000\000\000\000\071\000\000\000\
\081\000\082\000\072\000\108\000\000\000\073\000\074\000\000\000\
\000\000\000\000\075\000\000\000\000\000\000\000\076\000\077\000\
\069\000\070\000\078\000\079\000\000\000\000\000\071\000\000\000\
\081\000\082\000\072\000\000\000\000\000\073\000\074\000\069\000\
\000\000\000\000\075\000\000\000\000\000\071\000\076\000\077\000\
\000\000\072\000\078\000\079\000\073\000\074\000\069\000\000\000\
\081\000\082\000\000\000\000\000\071\000\076\000\077\000\000\000\
\072\000\078\000\079\000\073\000\074\000\000\000\000\000\081\000\
\082\000\000\000\000\000\071\000\076\000\077\000\000\000\072\000\
\000\000\079\000\073\000\074\000\000\000\000\000\081\000\082\000\
\000\000\000\000\000\000\076\000\077\000\000\000\000\000\000\000\
\079\000\051\000\052\000\053\000\000\000\081\000\082\000\000\000\
\000\000\000\000\054\000\055\000\000\000\000\000\000\000\056\000\
\057\000\000\000\000\000\058\000\000\000\000\000\059\000"

let yycheck = "\047\000\
\000\000\003\001\088\000\018\000\001\000\016\000\005\001\055\000\
\056\000\057\000\058\000\059\000\027\000\025\001\016\001\032\000\
\017\001\103\000\020\001\000\000\037\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\031\001\081\000\082\000\001\001\002\001\048\000\017\001\087\000\
\006\001\007\001\022\001\022\001\017\001\011\001\012\001\006\001\
\014\001\015\001\031\001\005\001\022\001\019\001\032\001\030\001\
\032\001\023\001\024\001\031\001\108\000\027\001\028\001\001\001\
\002\001\031\001\032\001\033\001\034\001\007\001\036\001\011\001\
\037\001\011\001\021\001\032\001\014\001\015\001\017\001\011\001\
\035\001\019\001\006\001\023\001\024\001\023\001\024\001\014\001\
\028\001\027\001\028\001\032\001\024\001\033\001\034\001\033\001\
\034\001\022\001\036\001\001\001\002\001\033\001\034\001\031\001\
\006\001\007\001\000\000\021\001\031\001\031\001\012\001\031\001\
\014\001\015\001\031\001\021\001\035\001\019\001\031\001\031\001\
\006\000\023\001\001\001\002\001\041\000\027\001\028\001\006\001\
\007\001\031\001\032\001\037\000\255\255\012\001\036\001\014\001\
\015\001\255\255\255\255\255\255\019\001\255\255\255\255\255\255\
\023\001\001\001\002\001\255\255\027\001\028\001\006\001\007\001\
\031\001\032\001\255\255\255\255\012\001\036\001\014\001\015\001\
\255\255\255\255\255\255\019\001\255\255\255\255\255\255\023\001\
\001\001\002\001\255\255\027\001\028\001\006\001\007\001\031\001\
\032\001\255\255\255\255\012\001\036\001\014\001\015\001\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\255\255\001\001\
\002\001\255\255\027\001\255\255\006\001\007\001\031\001\032\001\
\255\255\255\255\012\001\036\001\014\001\015\001\255\255\255\255\
\255\255\019\001\255\255\255\255\006\001\255\255\001\001\002\001\
\255\255\027\001\012\001\006\001\007\001\031\001\032\001\255\255\
\255\255\012\001\036\001\014\001\015\001\002\001\001\001\002\001\
\019\001\006\001\255\255\006\001\255\255\031\001\032\001\012\001\
\027\001\012\001\036\001\255\255\031\001\032\001\019\001\255\255\
\019\001\036\001\255\255\255\255\255\255\255\255\027\001\255\255\
\027\001\255\255\031\001\032\001\031\001\032\001\002\001\036\001\
\255\255\036\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\006\001\255\255\255\255\255\255\255\255\019\001\
\012\001\025\001\255\255\255\255\001\001\002\001\255\255\255\255\
\255\255\006\001\007\001\031\001\032\001\255\255\011\001\255\255\
\036\001\014\001\015\001\031\001\032\001\255\255\019\001\255\255\
\036\001\255\255\023\001\024\001\255\255\255\255\027\001\028\001\
\001\001\002\001\031\001\255\255\033\001\034\001\007\001\255\255\
\255\255\255\255\011\001\255\255\255\255\014\001\015\001\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\023\001\024\001\
\001\001\002\001\027\001\028\001\255\255\006\001\007\001\032\001\
\033\001\034\001\011\001\255\255\255\255\014\001\015\001\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\023\001\024\001\
\001\001\002\001\027\001\028\001\255\255\255\255\007\001\255\255\
\033\001\034\001\011\001\012\001\255\255\014\001\015\001\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\023\001\024\001\
\001\001\002\001\027\001\028\001\255\255\255\255\007\001\255\255\
\033\001\034\001\011\001\255\255\255\255\014\001\015\001\001\001\
\255\255\255\255\019\001\255\255\255\255\007\001\023\001\024\001\
\255\255\011\001\027\001\028\001\014\001\015\001\001\001\255\255\
\033\001\034\001\255\255\255\255\007\001\023\001\024\001\255\255\
\011\001\027\001\028\001\014\001\015\001\255\255\255\255\033\001\
\034\001\255\255\255\255\007\001\023\001\024\001\255\255\011\001\
\255\255\028\001\014\001\015\001\255\255\255\255\033\001\034\001\
\255\255\255\255\255\255\023\001\024\001\255\255\255\255\255\255\
\028\001\008\001\009\001\010\001\255\255\033\001\034\001\255\255\
\255\255\255\255\017\001\018\001\255\255\255\255\255\255\022\001\
\023\001\255\255\255\255\026\001\255\255\255\255\029\001"

let yynames_const = "\
  AND\000\
  ARROW\000\
  BOOL\000\
  CONST\000\
  COLON\000\
  COMMA\000\
  DIV\000\
  ELSE\000\
  END\000\
  EOF\000\
  EQUAL\000\
  NEQ\000\
  REAL\000\
  IF\000\
  IMPL\000\
  INT\000\
  LET\000\
  LPAREN\000\
  MINUS\000\
  MOD\000\
  NODE\000\
  NOT\000\
  OR\000\
  PLUS\000\
  PRE\000\
  RETURNS\000\
  RPAREN\000\
  SEMICOL\000\
  SLASH\000\
  STAR\000\
  TEL\000\
  THEN\000\
  VAR\000\
  "

let yynames_block = "\
  COMP\000\
  CONST_BOOL\000\
  CONST_INT\000\
  CONST_REAL\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node_decs) in
    Obj.repr(
# 72 "parser.mly"
                    ( _1 )
# 350 "parser.ml"
               : Parse_ast.p_file))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                    ( [] )
# 356 "parser.ml"
               : 'node_decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node_decs) in
    Obj.repr(
# 77 "parser.mly"
                    ( _1 :: _2 )
# 364 "parser.ml"
               : 'node_decs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 13 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 11 : 'in_params) in
    let _8 = (Parsing.peek_val __caml_parser_env 7 : 'out_params) in
    let _11 = (Parsing.peek_val __caml_parser_env 4 : 'local_params) in
    let _13 = (Parsing.peek_val __caml_parser_env 2 : 'eq_list) in
    let _15 = (Parsing.peek_val __caml_parser_env 0 : 'semi_opt) in
    Obj.repr(
# 86 "parser.mly"
    ( { pn_name = _2;
	pn_input = _4;
	pn_output = _8;
	pn_local = _11;
	pn_equs = _13;
	pn_loc = loc(); } )
# 381 "parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
    ( [] )
# 387 "parser.ml"
               : 'in_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 98 "parser.mly"
    ( _1 )
# 394 "parser.ml"
               : 'in_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 104 "parser.mly"
    ( _1 )
# 401 "parser.ml"
               : 'out_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
    ( [] )
# 407 "parser.ml"
               : 'local_params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param_list_semicol) in
    Obj.repr(
# 111 "parser.mly"
    ( _2 )
# 414 "parser.ml"
               : 'local_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 116 "parser.mly"
    ( _1 )
# 421 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 118 "parser.mly"
    ( _1 @ _3 )
# 429 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 123 "parser.mly"
    ( _1 )
# 436 "parser.ml"
               : 'param_list_semicol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list_semicol) in
    Obj.repr(
# 125 "parser.mly"
    ( _1 @ _3 )
# 444 "parser.ml"
               : 'param_list_semicol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 131 "parser.mly"
      ( let typ = _3 in
        List.map (fun id -> (id, typ)) _1 )
# 453 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq) in
    Obj.repr(
# 137 "parser.mly"
    ( [_1] )
# 460 "parser.ml"
               : 'eq_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'eq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eq_list) in
    Obj.repr(
# 139 "parser.mly"
    ( _1 :: _2 )
# 468 "parser.ml"
               : 'eq_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
    ( { peq_patt = _1; peq_expr = _3; } )
# 476 "parser.ml"
               : 'eq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "parser.mly"
    ( mk_patt (PP_ident _1) )
# 483 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ident_comma_list) in
    Obj.repr(
# 151 "parser.mly"
    ( mk_patt (PP_tuple(_2::_4)) )
# 491 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
    ( _2 )
# 498 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 158 "parser.mly"
    ( _1 )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parser.mly"
    ( mk_expr (PE_ident _1))
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_comma_list_empty) in
    Obj.repr(
# 162 "parser.mly"
    ( mk_expr (PE_app (_1, _3)))
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
    ( mk_expr (PE_op (Op_if, [_2; _4; _6])) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
    ( mk_expr (PE_op (Op_add, [_1; _3])) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
    ( mk_expr (PE_op (Op_sub, [_1; _3])) )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
    ( mk_expr (PE_op (Op_mul, [_1; _3])) )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
    ( mk_expr (PE_op (Op_div, [_1; _3])) )
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
    ( mk_expr (PE_op (Op_div, [_1; _3])) )
# 569 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
    ( mk_expr (PE_op (Op_mod, [_1; _3])) )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Asttypes.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
    ( mk_expr (PE_op (_2, [_1; _3])) )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
    ( mk_expr (PE_op (Op_eq, [_1; _3])) )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
    ( mk_expr (PE_op (Op_neq, [_1; _3])) )
# 602 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
    ( mk_expr (PE_op (Op_and, [_1; _3])) )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
    ( mk_expr (PE_op (Op_or, [_1; _3])) )
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
    ( mk_expr (PE_op (Op_impl, [_1; _3])) )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
    ( mk_expr (PE_arrow (_1, _3)) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
    ( mk_expr (PE_op (Op_sub, [_2])) )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
    ( mk_expr (PE_op (Op_not, [_2])) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
    ( mk_expr (PE_pre (_2)) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_comma_list) in
    Obj.repr(
# 198 "parser.mly"
    ( mk_expr (PE_tuple (_2::_4)) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 203 "parser.mly"
    ( mk_expr (PE_const (Cbool _1)) )
# 670 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 205 "parser.mly"
    ( mk_expr (PE_const (Cint _1)) )
# 677 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 207 "parser.mly"
    ( mk_expr (PE_const (Creal _1)) )
# 684 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_comma_list) in
    Obj.repr(
# 212 "parser.mly"
    ( _1 :: _3 )
# 692 "parser.ml"
               : 'ident_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 213 "parser.mly"
        ( [_1] )
# 699 "parser.ml"
               : 'ident_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 217 "parser.mly"
    ( [] )
# 705 "parser.ml"
               : 'expr_comma_list_empty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 218 "parser.mly"
                  ( _1 )
# 712 "parser.ml"
               : 'expr_comma_list_empty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 223 "parser.mly"
    ( _1 :: _3 )
# 720 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 224 "parser.mly"
       ( [_1] )
# 727 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 228 "parser.mly"
         ( Tbool )
# 733 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 229 "parser.mly"
         ( Tint )
# 739 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 230 "parser.mly"
         ( Treal )
# 745 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "parser.mly"
    ( () )
# 751 "parser.ml"
               : 'semi_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 236 "parser.mly"
    ( () )
# 757 "parser.ml"
               : 'semi_opt))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Parse_ast.p_file)
