type token =
  | INPUT
  | GATE
  | PRINT
  | TRUE
  | FALSE
  | WRITE
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
# 28 "pcfparse.ml"
let yytransl_const = [|
  257 (* INPUT *);
  258 (* GATE *);
  259 (* PRINT *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* WRITE *);
  263 (* NOT *);
  264 (* AND *);
  265 (* OR *);
  268 (* DOT *);
  269 (* EQUAL *);
  270 (* COMMA *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* LBRACE *);
  274 (* RBRACE *);
  275 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  266 (* IDENT *);
  267 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\010\000\010\000\006\000\006\000\011\000\011\000\
\007\000\013\000\013\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\008\000\008\000\004\000\004\000\009\000\015\000\
\015\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\005\000\003\000\011\000\007\000\008\000\
\007\000\002\000\002\000\000\000\002\000\000\000\004\000\007\000\
\002\000\002\000\000\000\003\000\002\000\003\000\003\000\002\000\
\001\000\001\000\002\000\000\000\001\000\001\000\002\000\002\000\
\000\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\002\000\000\000\005\000\000\000\000\000\000\000\000\000\029\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\025\000\
\026\000\000\000\000\000\000\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\024\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\027\000\
\000\000\034\000\009\000\032\000\020\000\007\000\000\000\000\000\
\018\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\006\000\013\000\000\000\000\000\000\000\015\000\000\000\000\000\
\016\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\026\000\028\000\078\000\045\000\056\000\
\038\000\034\000\079\000\046\000\062\000\039\000\054\000"

let yysindex = "\002\000\
\034\255\000\000\004\255\023\255\028\255\032\255\038\255\000\000\
\060\000\034\255\250\254\052\255\058\255\060\255\062\255\000\000\
\000\000\059\255\000\000\063\255\054\255\056\255\061\255\000\000\
\000\000\055\255\064\255\065\255\067\255\069\255\001\255\000\000\
\063\255\000\000\068\255\070\255\072\255\071\255\066\255\000\000\
\000\000\001\255\070\255\001\255\073\255\040\255\000\000\063\255\
\075\255\074\255\076\255\077\255\069\255\000\000\057\255\000\000\
\037\255\078\255\001\255\001\255\001\255\000\000\079\255\000\000\
\080\255\000\000\000\000\000\000\000\000\000\000\057\255\057\255\
\000\000\081\255\000\000\082\255\087\255\083\255\082\255\005\255\
\000\000\000\000\027\255\015\255\001\255\000\000\086\255\084\255\
\000\000"

let yyrindex = "\000\000\
\075\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\075\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\088\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\089\255\012\255\000\000\090\255\000\000\
\000\000\000\000\013\255\000\000\000\000\091\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\255\043\255\
\000\000\000\000\000\000\092\255\000\000\000\000\092\255\000\000\
\000\000\000\000\022\255\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\078\000\000\000\000\000\227\255\012\000\196\255\057\000\
\041\000\000\000\000\000\214\255\000\000\000\000\000\000"

let yytablesize = 110
let yytable = "\055\000\
\073\000\057\000\001\000\047\000\040\000\041\000\018\000\042\000\
\040\000\041\000\043\000\042\000\019\000\011\000\083\000\044\000\
\071\000\072\000\063\000\044\000\028\000\028\000\059\000\060\000\
\087\000\035\000\028\000\035\000\028\000\028\000\028\000\028\000\
\012\000\086\000\003\000\004\000\005\000\084\000\049\000\006\000\
\028\000\085\000\013\000\007\000\059\000\060\000\014\000\059\000\
\060\000\021\000\015\000\021\000\069\000\061\000\021\000\022\000\
\023\000\022\000\023\000\016\000\022\000\023\000\024\000\025\000\
\059\000\060\000\020\000\029\000\021\000\030\000\022\000\023\000\
\027\000\032\000\003\000\031\000\036\000\033\000\037\000\053\000\
\035\000\049\000\048\000\051\000\064\000\066\000\052\000\017\000\
\058\000\065\000\082\000\077\000\050\000\068\000\074\000\067\000\
\070\000\076\000\075\000\080\000\081\000\088\000\089\000\012\000\
\028\000\033\000\019\000\000\000\000\000\014\000"

let yycheck = "\042\000\
\061\000\044\000\001\000\033\000\004\001\005\001\013\001\007\001\
\004\001\005\001\010\001\007\001\019\001\010\001\010\001\015\001\
\059\000\060\000\048\000\015\001\008\001\009\001\008\001\009\001\
\085\000\014\001\014\001\016\001\016\001\008\001\009\001\019\001\
\010\001\019\001\001\001\002\001\003\001\080\000\012\001\006\001\
\019\001\015\001\015\001\010\001\008\001\009\001\015\001\008\001\
\009\001\014\001\013\001\016\001\016\001\014\001\019\001\014\001\
\014\001\016\001\016\001\000\000\019\001\019\001\004\001\005\001\
\008\001\009\001\015\001\014\001\011\001\014\001\011\001\010\001\
\010\001\019\001\000\000\015\001\010\001\014\001\010\001\014\001\
\016\001\012\001\015\001\012\001\010\001\010\001\016\001\010\000\
\016\001\016\001\079\000\010\001\036\000\053\000\016\001\019\001\
\019\001\017\001\019\001\013\001\018\001\016\001\019\001\016\001\
\016\001\016\001\016\001\255\255\255\255\018\001"

let yynames_const = "\
  INPUT\000\
  GATE\000\
  PRINT\000\
  TRUE\000\
  FALSE\000\
  WRITE\000\
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
# 17 "pcfparse.mly"
                               ( _1 )
# 185 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 20 "pcfparse.mly"
                               ( _1 :: _2 )
# 193 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "pcfparse.mly"
                               ( [] )
# 199 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'boolval) in
    Obj.repr(
# 25 "pcfparse.mly"
      ( InputDecl(_2, Some _4) )
# 207 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 27 "pcfparse.mly"
      ( InputDecl(_2, None) )
# 214 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'params) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 30 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 224 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'callargs) in
    Obj.repr(
# 33 "pcfparse.mly"
      (
        let actual_strings =
          List.map
            (function
              | Var(id, None)     -> id
              | Var(id, Some sub) -> id ^ "." ^ sub
              | _  -> failwith "InstDecl : chaque argument doit être un ident ou ident.sous_ident"
            )
            _5
        in
        InstDecl(_1, _3, actual_strings)
      )
# 244 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idnext) in
    Obj.repr(
# 46 "pcfparse.mly"
      ( PrintStmt(_3, _5, _6) )
# 253 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'wtargets) in
    Obj.repr(
# 49 "pcfparse.mly"
      ( WriteStmt(_3, _5) )
# 261 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 52 "pcfparse.mly"
                               ( _1 :: _2 )
# 269 "pcfparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 55 "pcfparse.mly"
                               ( _2 )
# 276 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "pcfparse.mly"
                               ( [] )
# 282 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 59 "pcfparse.mly"
                               ( _1 :: _2 )
# 290 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "pcfparse.mly"
                               ( [] )
# 296 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "pcfparse.mly"
      ( Assign(_1, _3) )
# 304 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'callargs) in
    Obj.repr(
# 66 "pcfparse.mly"
      (
        InstAssign(_1, _3, _5)
      )
# 315 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_cargs) in
    Obj.repr(
# 71 "pcfparse.mly"
                               ( _1 :: _2 )
# 323 "pcfparse.ml"
               : 'callargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'callargs) in
    Obj.repr(
# 74 "pcfparse.mly"
                               ( _2 )
# 330 "pcfparse.ml"
               : 'more_cargs))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "pcfparse.mly"
                               ( [] )
# 336 "pcfparse.ml"
               : 'more_cargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "pcfparse.mly"
                               ( Parens(_2) )
# 343 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "pcfparse.mly"
                               ( Not(_2) )
# 350 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "pcfparse.mly"
                               ( And(_1, _3) )
# 358 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "pcfparse.mly"
                               ( Or(_1, _3) )
# 366 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idnext) in
    Obj.repr(
# 82 "pcfparse.mly"
                               ( Var(_1, _2) )
# 374 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "pcfparse.mly"
                               ( True )
# 380 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "pcfparse.mly"
                               ( False )
# 386 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "pcfparse.mly"
                               ( Some(_2) )
# 393 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "pcfparse.mly"
                               ( None )
# 399 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "pcfparse.mly"
                               ( true )
# 405 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "pcfparse.mly"
                               ( false )
# 411 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'wtarget) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_wtargets) in
    Obj.repr(
# 95 "pcfparse.mly"
                               ( _1 :: _2 )
# 419 "pcfparse.ml"
               : 'wtargets))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'wtargets) in
    Obj.repr(
# 98 "pcfparse.mly"
                               ( _2 )
# 426 "pcfparse.ml"
               : 'more_wtargets))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "pcfparse.mly"
                               ( [] )
# 432 "pcfparse.ml"
               : 'more_wtargets))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "pcfparse.mly"
                               ( TSignal(_1, Some _3) )
# 440 "pcfparse.ml"
               : 'wtarget))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "pcfparse.mly"
                               ( TSignal(_1, None) )
# 447 "pcfparse.ml"
               : 'wtarget))
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
