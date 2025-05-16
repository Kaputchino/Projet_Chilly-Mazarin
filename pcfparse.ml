type token =
  | INPUT
  | GATE
  | PRINT
  | TRUE
  | FALSE
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

open Parsing;;
let _ = parse_error;;
# 2 "pcfparse.mly"
open Pcfast
# 27 "pcfparse.ml"
let yytransl_const = [|
  257 (* INPUT *);
  258 (* GATE *);
  259 (* PRINT *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* NOT *);
  263 (* AND *);
  264 (* OR *);
  267 (* DOT *);
  268 (* EQUAL *);
  269 (* COMMA *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LBRACE *);
  273 (* RBRACE *);
  274 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  265 (* IDENT *);
  266 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\007\000\
\007\000\005\000\005\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\003\000\011\000\008\000\002\000\002\000\
\000\000\002\000\000\000\004\000\003\000\002\000\002\000\003\000\
\003\000\001\000\001\000\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\001\000\002\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\005\000\010\000\018\000\
\019\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\012\000\013\000\000\000\000\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\018\000\035\000\027\000\021\000\036\000\
\045\000"

let yysindex = "\006\000\
\030\255\000\000\019\255\025\255\021\255\000\000\036\000\030\255\
\020\255\023\255\029\255\000\000\000\000\000\000\031\255\028\255\
\032\255\027\255\034\255\031\255\000\000\033\255\035\255\000\000\
\031\255\039\255\036\255\037\255\000\000\026\255\038\255\000\000\
\040\255\041\255\042\255\040\255\000\255\000\000\000\000\000\000\
\000\000\000\255\035\255\000\255\251\254\013\255\000\000\008\255\
\000\255\000\255\000\000\000\000\013\255\013\255"

let yyrindex = "\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\255\000\000\000\000\000\000\000\000\000\000\045\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\044\255\000\000\000\000\044\255\000\000\000\000\000\000\000\000\
\000\000\000\000\249\254\000\000\000\000\007\255\000\000\000\000\
\000\000\000\000\000\000\000\000\011\255\012\255"

let yygindex = "\000\000\
\000\000\047\000\000\000\255\255\020\000\014\000\000\000\000\000\
\224\255"

let yytablesize = 61
let yytable = "\021\000\
\021\000\049\000\050\000\040\000\041\000\042\000\001\000\021\000\
\043\000\046\000\021\000\048\000\051\000\044\000\049\000\050\000\
\053\000\054\000\024\000\049\000\050\000\014\000\052\000\028\000\
\014\000\016\000\017\000\009\000\016\000\017\000\003\000\004\000\
\005\000\010\000\011\000\012\000\015\000\014\000\016\000\017\000\
\019\000\022\000\023\000\032\000\020\000\026\000\025\000\029\000\
\034\000\003\000\030\000\031\000\037\000\033\000\013\000\039\000\
\047\000\009\000\038\000\021\000\011\000"

let yycheck = "\007\001\
\008\001\007\001\008\001\004\001\005\001\006\001\001\000\015\001\
\009\001\042\000\018\001\044\000\018\001\014\001\007\001\008\001\
\049\000\050\000\020\000\007\001\008\001\015\001\015\001\025\000\
\018\001\015\001\015\001\009\001\018\001\018\001\001\001\002\001\
\003\001\009\001\014\001\000\000\014\001\018\001\010\001\009\001\
\013\001\015\001\009\001\018\001\013\001\011\001\014\001\009\001\
\009\001\000\000\015\001\015\001\012\001\016\001\008\000\036\000\
\043\000\015\001\017\001\015\001\017\001"

let yynames_const = "\
  INPUT\000\
  GATE\000\
  PRINT\000\
  TRUE\000\
  FALSE\000\
  NOT\000\
  AND\000\
  OR\000\
  DOT\000\
  EQUAL\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 18 "pcfparse.mly"
              ( _1 )
# 151 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 21 "pcfparse.mly"
               ( _1 :: _2 )
# 159 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "pcfparse.mly"
                ( [] )
# 165 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 26 "pcfparse.mly"
      ( InputDecl(_2) )
# 172 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'params) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 29 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 182 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idnext) in
    Obj.repr(
# 32 "pcfparse.mly"
      ( PrintStmt(_3, _5, _6) )
# 191 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 35 "pcfparse.mly"
                      ( _1 :: _2 )
# 199 "pcfparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 38 "pcfparse.mly"
                 ( _2 )
# 206 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "pcfparse.mly"
                  ( [] )
# 212 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 42 "pcfparse.mly"
               ( _1 :: _2 )
# 220 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "pcfparse.mly"
                ( [] )
# 226 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "pcfparse.mly"
      ( Assign(_1, _3) )
# 234 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "pcfparse.mly"
                           ( Parens(_2) )
# 241 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "pcfparse.mly"
                           ( Not(_2) )
# 248 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idnext) in
    Obj.repr(
# 52 "pcfparse.mly"
                           ( Var(_1, _2) )
# 256 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "pcfparse.mly"
                           ( And(_1, _3) )
# 264 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "pcfparse.mly"
                           ( Or(_1, _3) )
# 272 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "pcfparse.mly"
                           ( True )
# 278 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "pcfparse.mly"
                           ( False )
# 284 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "pcfparse.mly"
                           ( Some(_2) )
# 291 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "pcfparse.mly"
                           ( None )
# 297 "pcfparse.ml"
               : 'idnext))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Pcfast.program)
