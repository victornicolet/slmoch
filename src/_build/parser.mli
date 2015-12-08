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

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parse_ast.p_file
