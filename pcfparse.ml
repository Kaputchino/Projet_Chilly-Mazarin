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

open Parsing;;
let _ = parse_error;;
# 3 "pcfparse.mly"
open Pcfast
# 29 "pcfparse.ml"
let yytransl_const = [|
  257 (* INPUT *);
  258 (* GATE *);
  259 (* PRINT *);
  260 (* WRITE *);
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* UNDET *);
  264 (* NOT *);
  265 (* AND *);
  266 (* OR *);
  269 (* DOT *);
  270 (* EQUAL *);
  271 (* COMMA *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* LBRACE *);
  275 (* RBRACE *);
  276 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* IDENT *);
  268 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\010\000\010\000\009\000\012\000\012\000\011\000\
\011\000\008\000\008\000\004\000\004\000\004\000\006\000\006\000\
\013\000\013\000\007\000\015\000\015\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\003\000\005\000\011\000\007\000\007\000\
\007\000\002\000\002\000\000\000\002\000\002\000\000\000\003\000\
\001\000\003\000\001\000\001\000\001\000\001\000\002\000\000\000\
\004\000\007\000\002\000\002\000\000\000\003\000\002\000\003\000\
\003\000\002\000\001\000\001\000\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\000\000\004\000\000\000\000\000\000\000\000\000\020\000\
\021\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\018\000\008\000\016\000\009\000\014\000\037\000\
\030\000\007\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\023\000\000\000\000\000\000\000\
\025\000\000\000\000\000\026\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\027\000\029\000\081\000\047\000\038\000\
\040\000\035\000\041\000\056\000\082\000\048\000\065\000\059\000"

let yysindex = "\016\000\
\044\255\000\000\042\255\048\255\027\255\045\255\053\255\000\000\
\062\000\044\255\245\254\052\255\057\255\058\255\060\255\000\000\
\000\000\059\255\000\000\061\255\062\255\063\255\064\255\000\000\
\000\000\000\000\054\255\066\255\056\255\065\255\068\255\000\255\
\000\000\061\255\000\000\067\255\069\255\070\255\071\255\072\255\
\073\255\000\000\000\000\000\255\077\255\000\255\074\255\041\255\
\000\000\061\255\075\255\055\255\081\255\076\255\068\255\000\000\
\005\255\082\255\000\000\032\255\078\255\000\255\000\255\000\255\
\000\000\080\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\005\255\000\000\083\255\084\255\085\255\
\086\255\084\255\016\255\000\000\000\000\250\254\003\255\000\255\
\000\000\087\255\088\255\000\000"

let yyrindex = "\000\000\
\085\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\085\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\089\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\090\255\000\000\021\255\000\000\
\092\255\000\000\000\000\000\000\020\255\000\000\000\000\093\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\255\043\255\000\000\000\000\094\255\000\000\
\000\000\094\255\000\000\000\000\000\000\024\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\084\000\000\000\000\000\226\255\018\000\193\255\000\000\
\047\000\000\000\000\000\000\000\000\000\212\255\000\000\000\000"

let yytablesize = 113
let yytable = "\057\000\
\077\000\060\000\018\000\049\000\042\000\043\000\058\000\044\000\
\019\000\088\000\045\000\062\000\063\000\062\000\063\000\046\000\
\001\000\075\000\076\000\066\000\042\000\043\000\089\000\044\000\
\090\000\031\000\086\000\031\000\038\000\038\000\031\000\046\000\
\038\000\038\000\038\000\017\000\038\000\017\000\087\000\038\000\
\062\000\063\000\013\000\038\000\003\000\004\000\005\000\006\000\
\073\000\062\000\063\000\032\000\011\000\032\000\007\000\064\000\
\032\000\033\000\012\000\033\000\014\000\016\000\033\000\024\000\
\025\000\026\000\015\000\020\000\021\000\022\000\023\000\028\000\
\036\000\033\000\068\000\037\000\030\000\031\000\039\000\032\000\
\034\000\051\000\050\000\053\000\003\000\067\000\052\000\055\000\
\054\000\058\000\061\000\069\000\072\000\017\000\080\000\070\000\
\078\000\074\000\083\000\085\000\079\000\071\000\000\000\091\000\
\084\000\012\000\019\000\092\000\015\000\029\000\000\000\000\000\
\024\000"

let yycheck = "\044\000\
\064\000\046\000\014\001\034\000\005\001\006\001\013\001\008\001\
\020\001\016\001\011\001\009\001\010\001\009\001\010\001\016\001\
\001\000\062\000\063\000\050\000\005\001\006\001\020\001\008\001\
\088\000\015\001\011\001\017\001\009\001\010\001\020\001\016\001\
\009\001\010\001\015\001\015\001\017\001\017\001\083\000\020\001\
\009\001\010\001\016\001\020\001\001\001\002\001\003\001\004\001\
\017\001\009\001\010\001\015\001\011\001\017\001\011\001\015\001\
\020\001\015\001\011\001\017\001\016\001\000\000\020\001\005\001\
\006\001\007\001\014\001\016\001\012\001\012\001\011\001\011\001\
\017\001\020\001\020\001\011\001\015\001\015\001\011\001\016\001\
\015\001\013\001\016\001\013\001\000\000\011\001\017\001\015\001\
\017\001\013\001\017\001\011\001\011\001\010\000\011\001\020\001\
\017\001\020\001\014\001\082\000\018\001\055\000\255\255\017\001\
\019\001\017\001\017\001\020\001\017\001\017\001\255\255\255\255\
\019\001"

let yynames_const = "\
  INPUT\000\
  GATE\000\
  PRINT\000\
  WRITE\000\
  TRUE\000\
  FALSE\000\
  UNDET\000\
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
# 190 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 22 "pcfparse.mly"
               ( _1 :: _2 )
# 198 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "pcfparse.mly"
               ( [] )
# 204 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 28 "pcfparse.mly"
      ( InputDecl(_2, None) )
# 211 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'boollit) in
    Obj.repr(
# 30 "pcfparse.mly"
      ( InputDecl(_2, Some _4) )
# 219 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'idlist) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'idlist) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 35 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 229 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'callargs) in
    Obj.repr(
# 39 "pcfparse.mly"
      ( let ids =
          List.map (function
              | Var(id,None)     -> id
              | Var(id,Some sub) -> id ^ "." ^ sub
              | _ -> failwith "global InstDecl: args simples") _5
        in InstDecl(_1,_3,ids) )
# 243 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'ident_or_gate) in
    Obj.repr(
# 48 "pcfparse.mly"
      ( let (id,sub) = _5 in PrintStmt(_3,id,sub) )
# 251 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'wtargets) in
    Obj.repr(
# 51 "pcfparse.mly"
      ( WriteStmt(_3,_5) )
# 259 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_ids) in
    Obj.repr(
# 55 "pcfparse.mly"
                   ( _1 :: _2 )
# 267 "pcfparse.ml"
               : 'idlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idlist) in
    Obj.repr(
# 57 "pcfparse.mly"
                 ( _2 )
# 274 "pcfparse.ml"
               : 'more_ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "pcfparse.mly"
                          ( [] )
# 280 "pcfparse.ml"
               : 'more_ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'wtarget) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_wt) in
    Obj.repr(
# 60 "pcfparse.mly"
                    ( _1 :: _2 )
# 288 "pcfparse.ml"
               : 'wtargets))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'wtargets) in
    Obj.repr(
# 62 "pcfparse.mly"
                   ( _2 )
# 295 "pcfparse.ml"
               : 'more_wt))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "pcfparse.mly"
                            ( [] )
# 301 "pcfparse.ml"
               : 'more_wt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "pcfparse.mly"
                    ( TSignal(_1,Some _3) )
# 309 "pcfparse.ml"
               : 'wtarget))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "pcfparse.mly"
                    ( TSignal(_1,None) )
# 316 "pcfparse.ml"
               : 'wtarget))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "pcfparse.mly"
                    ( (_1,Some _3) )
# 324 "pcfparse.ml"
               : 'ident_or_gate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "pcfparse.mly"
                    ( (_1,None) )
# 331 "pcfparse.ml"
               : 'ident_or_gate))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "pcfparse.mly"
          ( TTrue )
# 337 "pcfparse.ml"
               : 'boollit))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "pcfparse.mly"
                            ( TFalse )
# 343 "pcfparse.ml"
               : 'boollit))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "pcfparse.mly"
                                               ( TUndet )
# 349 "pcfparse.ml"
               : 'boollit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 76 "pcfparse.mly"
               ( _1 :: _2 )
# 357 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "pcfparse.mly"
                              ( [] )
# 363 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "pcfparse.mly"
      ( Assign(_1,_3) )
# 371 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'callargs) in
    Obj.repr(
# 82 "pcfparse.mly"
      ( InstAssign(_1,_3,_5) )
# 380 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_cargs) in
    Obj.repr(
# 86 "pcfparse.mly"
                    ( _1 :: _2 )
# 388 "pcfparse.ml"
               : 'callargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'callargs) in
    Obj.repr(
# 88 "pcfparse.mly"
                   ( _2 )
# 395 "pcfparse.ml"
               : 'more_cargs))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "pcfparse.mly"
                            ( [] )
# 401 "pcfparse.ml"
               : 'more_cargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "pcfparse.mly"
                           ( Parens(_2) )
# 408 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "pcfparse.mly"
                           ( Not(_2) )
# 415 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "pcfparse.mly"
                           ( And(_1,_3) )
# 423 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "pcfparse.mly"
                           ( Or (_1,_3) )
# 431 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'after_ident) in
    Obj.repr(
# 96 "pcfparse.mly"
                           ( Var(_1,_2) )
# 439 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "pcfparse.mly"
                           ( True )
# 445 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "pcfparse.mly"
                           ( False )
# 451 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "pcfparse.mly"
              ( Some _2 )
# 458 "pcfparse.ml"
               : 'after_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "pcfparse.mly"
                            ( None )
# 464 "pcfparse.ml"
               : 'after_ident))
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
