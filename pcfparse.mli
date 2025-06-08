type token =
  | INPUT
  | GATE
  | PRINT
  | WRITE
  | TRUE
  | FALSE
  | UNDET
  | NOT
  | AND
  | OR
  | IDENT of (string)
  | STRING of (string)
  | DOT
  | EQUAL
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMICOLON
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Pcfast.program
