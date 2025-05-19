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
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\005\000\009\000\009\000\006\000\006\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\007\000\007\000\004\000\
\004\000\008\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\005\000\003\000\011\000\008\000\007\000\
\002\000\002\000\000\000\002\000\000\000\004\000\003\000\002\000\
\002\000\003\000\003\000\001\000\001\000\002\000\000\000\001\000\
\001\000\002\000\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\002\000\000\000\
\005\000\000\000\000\000\000\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\009\000\000\000\000\000\
\000\000\000\000\010\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\022\000\000\000\027\000\008\000\000\000\007\000\
\000\000\000\000\000\000\000\000\000\000\006\000\012\000\020\000\
\021\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\014\000\015\000\000\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\023\000\025\000\051\000\038\000\034\000\
\030\000\052\000\061\000\040\000"

let yysindex = "\028\000\
\014\255\000\000\023\255\031\255\025\255\029\255\000\000\042\000\
\014\255\012\255\032\255\033\255\035\255\000\000\000\000\017\255\
\000\000\036\255\034\255\037\255\000\000\000\000\038\255\039\255\
\040\255\041\255\044\255\000\000\036\255\000\000\043\255\047\255\
\046\255\045\255\000\000\036\255\052\255\048\255\044\255\000\000\
\049\255\050\255\000\000\051\255\000\000\000\000\054\255\000\000\
\053\255\042\255\055\255\053\255\000\255\000\000\000\000\000\000\
\000\000\000\255\047\255\000\255\251\254\030\255\000\000\020\255\
\000\255\000\255\000\000\000\000\030\255\030\255"

let yyrindex = "\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\056\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\058\255\
\059\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\060\255\000\000\000\000\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\249\254\000\000\000\000\248\254\000\000\000\000\
\000\000\000\000\000\000\000\000\016\255\018\255"

let yygindex = "\000\000\
\000\000\040\000\000\000\000\000\239\255\012\000\007\000\029\000\
\000\000\000\000\216\255\000\000"

let yytablesize = 77
let yytable = "\023\000\
\023\000\065\000\066\000\056\000\057\000\058\000\016\000\023\000\
\059\000\016\000\023\000\035\000\067\000\060\000\003\000\004\000\
\005\000\062\000\042\000\064\000\021\000\022\000\006\000\016\000\
\069\000\070\000\065\000\066\000\001\000\017\000\018\000\010\000\
\019\000\018\000\068\000\019\000\065\000\066\000\012\000\011\000\
\013\000\014\000\019\000\020\000\024\000\018\000\026\000\003\000\
\015\000\032\000\027\000\029\000\033\000\053\000\031\000\028\000\
\036\000\037\000\039\000\041\000\043\000\050\000\044\000\055\000\
\047\000\063\000\046\000\045\000\048\000\049\000\011\000\054\000\
\023\000\028\000\000\000\000\000\013\000"

let yycheck = "\007\001\
\008\001\007\001\008\001\004\001\005\001\006\001\015\001\015\001\
\009\001\018\001\018\001\029\000\018\001\014\001\001\001\002\001\
\003\001\058\000\036\000\060\000\004\001\005\001\009\001\012\001\
\065\000\066\000\007\001\008\001\001\000\018\001\015\001\009\001\
\015\001\018\001\015\001\018\001\007\001\008\001\014\001\009\001\
\012\001\000\000\010\001\009\001\009\001\014\001\013\001\000\000\
\009\000\009\001\014\001\013\001\009\001\012\001\015\001\018\001\
\014\001\011\001\013\001\015\001\009\001\009\001\015\001\052\000\
\015\001\059\000\018\001\039\000\018\001\016\001\015\001\017\001\
\015\001\015\001\255\255\255\255\017\001"

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
# 163 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 21 "pcfparse.mly"
               ( _1 :: _2 )
# 171 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "pcfparse.mly"
                ( [] )
# 177 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'boolval) in
    Obj.repr(
# 26 "pcfparse.mly"
      ( InputDecl(_2, Some _4) )
# 185 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 29 "pcfparse.mly"
      ( InputDecl(_2, None) )
# 192 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'params) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 32 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 202 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idnext) in
    Obj.repr(
# 35 "pcfparse.mly"
      ( PrintStmt(_3, _5, _6) )
# 211 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    Obj.repr(
# 37 "pcfparse.mly"
      ( InstDecl(_1, _3, _5) )
# 220 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 40 "pcfparse.mly"
                      ( _1 :: _2 )
# 228 "pcfparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 42 "pcfparse.mly"
                 ( _2 )
# 235 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "pcfparse.mly"
                ( [] )
# 241 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 46 "pcfparse.mly"
               ( _1 :: _2 )
# 249 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "pcfparse.mly"
                ( [] )
# 255 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "pcfparse.mly"
      ( Assign(_1, _3) )
# 263 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "pcfparse.mly"
                           ( Parens(_2) )
# 270 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "pcfparse.mly"
                           ( Not(_2) )
# 277 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idnext) in
    Obj.repr(
# 56 "pcfparse.mly"
                           ( Var(_1, _2) )
# 285 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "pcfparse.mly"
                           ( And(_1, _3) )
# 293 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "pcfparse.mly"
                           ( Or(_1, _3) )
# 301 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "pcfparse.mly"
                           ( True )
# 307 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "pcfparse.mly"
                           ( False )
# 313 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "pcfparse.mly"
                           ( Some(_2) )
# 320 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "pcfparse.mly"
                           ( None )
# 326 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "pcfparse.mly"
          ( true )
# 332 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "pcfparse.mly"
          ( false )
# 338 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 70 "pcfparse.mly"
                    ( _1 :: _2 )
# 346 "pcfparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 72 "pcfparse.mly"
               ( _2 )
# 353 "pcfparse.ml"
               : 'more_args))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "pcfparse.mly"
                ( [] )
# 359 "pcfparse.ml"
               : 'more_args))
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
