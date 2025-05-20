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
  | WRITE
  | FILL

open Parsing;;
let _ = parse_error;;
# 2 "pcfparse.mly"
open Pcfast
# 29 "pcfparse.ml"
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
  275 (* WRITE *);
  276 (* FILL *);
    0|]

let yytransl_block = [|
  265 (* IDENT *);
  266 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\006\000\006\000\005\000\005\000\
\003\000\003\000\003\000\003\000\003\000\003\000\008\000\012\000\
\012\000\009\000\009\000\013\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\010\000\010\000\007\000\007\000\011\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\002\000\002\000\000\000\003\000\001\000\
\005\000\003\000\011\000\008\000\007\000\007\000\002\000\002\000\
\000\000\002\000\000\000\004\000\003\000\002\000\002\000\003\000\
\003\000\001\000\001\000\002\000\000\000\001\000\001\000\002\000\
\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\000\000\010\000\000\000\000\000\000\000\000\000\030\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\032\000\000\000\
\000\000\000\000\000\000\004\000\000\000\028\000\000\000\033\000\
\013\000\007\000\014\000\005\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\011\000\018\000\026\000\027\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\020\000\021\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\040\000\041\000\052\000\026\000\028\000\
\065\000\045\000\038\000\034\000\066\000\075\000\047\000"

let yysindex = "\011\000\
\255\254\000\000\013\255\029\255\014\255\032\255\031\255\000\000\
\046\000\255\254\253\254\033\255\038\255\040\255\041\255\000\000\
\000\000\036\255\000\000\043\255\037\255\039\255\042\255\000\000\
\000\000\044\255\045\255\046\255\047\255\048\255\050\255\000\000\
\043\255\000\000\049\255\053\255\052\255\051\255\056\255\054\255\
\055\255\000\000\043\255\061\255\057\255\048\255\000\000\058\255\
\062\255\059\255\050\255\000\000\060\255\000\000\063\255\000\000\
\000\000\000\000\000\000\000\000\064\255\000\000\065\255\066\255\
\067\255\065\255\015\255\000\000\000\000\000\000\000\000\015\255\
\053\255\015\255\254\254\035\255\000\000\019\255\015\255\015\255\
\000\000\000\000\035\255\035\255"

let yyrindex = "\000\000\
\054\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\068\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\070\255\071\255\000\000\024\255\000\000\
\072\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\073\255\000\000\
\000\000\073\255\000\000\000\000\000\000\000\000\000\000\000\000\
\252\254\000\000\000\000\251\254\000\000\000\000\000\000\000\000\
\000\000\000\000\017\255\018\255"

let yygindex = "\000\000\
\000\000\050\000\000\000\022\000\000\000\000\000\000\000\230\255\
\013\000\009\000\042\000\000\000\000\000\207\255\000\000"

let yytablesize = 90
let yytable = "\003\000\
\004\000\005\000\029\000\029\000\079\000\080\000\042\000\006\000\
\018\000\022\000\029\000\001\000\022\000\029\000\019\000\081\000\
\053\000\007\000\070\000\071\000\072\000\011\000\076\000\073\000\
\078\000\079\000\080\000\013\000\074\000\083\000\084\000\024\000\
\025\000\082\000\024\000\025\000\008\000\012\000\008\000\024\000\
\025\000\079\000\080\000\014\000\015\000\016\000\020\000\021\000\
\022\000\029\000\023\000\027\000\030\000\003\000\031\000\036\000\
\037\000\033\000\039\000\017\000\035\000\032\000\043\000\044\000\
\046\000\048\000\049\000\051\000\050\000\054\000\058\000\055\000\
\060\000\064\000\061\000\057\000\059\000\067\000\069\000\063\000\
\062\000\077\000\017\000\068\000\029\000\034\000\006\000\056\000\
\000\000\019\000"

let yycheck = "\001\001\
\002\001\003\001\007\001\008\001\007\001\008\001\033\000\009\001\
\012\001\015\001\015\001\001\000\018\001\018\001\018\001\018\001\
\043\000\019\001\004\001\005\001\006\001\009\001\072\000\009\001\
\074\000\007\001\008\001\014\001\014\001\079\000\080\000\015\001\
\015\001\015\001\018\001\018\001\013\001\009\001\015\001\004\001\
\005\001\007\001\008\001\012\001\014\001\000\000\014\001\010\001\
\009\001\013\001\010\001\009\001\014\001\000\000\013\001\009\001\
\009\001\013\001\009\001\010\000\015\001\018\001\014\001\011\001\
\013\001\015\001\011\001\013\001\015\001\009\001\009\001\015\001\
\051\000\009\001\015\001\018\001\018\001\012\001\066\000\016\001\
\018\001\073\000\015\001\017\001\015\001\015\001\015\001\046\000\
\255\255\017\001"

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
  WRITE\000\
  FILL\000\
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
# 19 "pcfparse.mly"
              ( _1 )
# 181 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 22 "pcfparse.mly"
               ( _1 :: _2 )
# 189 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "pcfparse.mly"
                ( [] )
# 195 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'wtarget) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_wtargets) in
    Obj.repr(
# 27 "pcfparse.mly"
                          ( _1 :: _2 )
# 203 "pcfparse.ml"
               : 'wtargets))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'wtargets) in
    Obj.repr(
# 29 "pcfparse.mly"
                   ( _2 )
# 210 "pcfparse.ml"
               : 'more_wtargets))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "pcfparse.mly"
                   ( [] )
# 216 "pcfparse.ml"
               : 'more_wtargets))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "pcfparse.mly"
                    ( TSignal(_1, Some _3) )
# 224 "pcfparse.ml"
               : 'wtarget))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "pcfparse.mly"
                    ( TSignal(_1, None) )
# 231 "pcfparse.ml"
               : 'wtarget))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'boolval) in
    Obj.repr(
# 40 "pcfparse.mly"
      ( InputDecl(_2, Some _4) )
# 239 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "pcfparse.mly"
      ( InputDecl(_2, None) )
# 246 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'params) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 46 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 256 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idnext) in
    Obj.repr(
# 49 "pcfparse.mly"
      ( PrintStmt(_3, _5, _6) )
# 265 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    Obj.repr(
# 51 "pcfparse.mly"
      ( InstDecl(_1, _3, _5) )
# 274 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'wtargets) in
    Obj.repr(
# 53 "pcfparse.mly"
      ( WriteStmt(_3, _5) )
# 282 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 57 "pcfparse.mly"
                      ( _1 :: _2 )
# 290 "pcfparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 59 "pcfparse.mly"
                 ( _2 )
# 297 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "pcfparse.mly"
                ( [] )
# 303 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 63 "pcfparse.mly"
               ( _1 :: _2 )
# 311 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "pcfparse.mly"
                ( [] )
# 317 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "pcfparse.mly"
      ( Assign(_1, _3) )
# 325 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "pcfparse.mly"
                           ( Parens(_2) )
# 332 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "pcfparse.mly"
                           ( Not(_2) )
# 339 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idnext) in
    Obj.repr(
# 73 "pcfparse.mly"
                           ( Var(_1, _2) )
# 347 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "pcfparse.mly"
                           ( And(_1, _3) )
# 355 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "pcfparse.mly"
                           ( Or(_1, _3) )
# 363 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "pcfparse.mly"
                           ( True )
# 369 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "pcfparse.mly"
                           ( False )
# 375 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "pcfparse.mly"
                           ( Some(_2) )
# 382 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "pcfparse.mly"
                           ( None )
# 388 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "pcfparse.mly"
          ( true )
# 394 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "pcfparse.mly"
          ( false )
# 400 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 87 "pcfparse.mly"
                    ( _1 :: _2 )
# 408 "pcfparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 89 "pcfparse.mly"
               ( _2 )
# 415 "pcfparse.ml"
               : 'more_args))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "pcfparse.mly"
                ( [] )
# 421 "pcfparse.ml"
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
