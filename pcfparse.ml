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
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\005\000\
\008\000\008\000\006\000\006\000\009\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\007\000\007\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\005\000\003\000\011\000\008\000\002\000\
\002\000\000\000\002\000\000\000\004\000\003\000\002\000\002\000\
\003\000\003\000\001\000\001\000\002\000\000\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\001\000\002\000\000\000\005\000\000\000\
\000\000\023\000\024\000\000\000\000\000\000\000\000\000\004\000\
\000\000\008\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\000\000\006\000\011\000\019\000\020\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\013\000\
\014\000\000\000\000\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\020\000\022\000\040\000\032\000\026\000\
\041\000\050\000"

let yysindex = "\006\000\
\032\255\000\000\011\255\021\255\022\255\000\000\022\000\032\255\
\007\255\025\255\030\255\000\000\000\000\033\255\000\000\035\255\
\036\255\000\000\000\000\027\255\037\255\028\255\038\255\000\000\
\035\255\000\000\039\255\040\255\000\000\035\255\043\255\031\255\
\041\255\000\000\042\255\045\255\000\000\046\255\047\255\048\255\
\046\255\000\255\000\000\000\000\000\000\000\000\000\255\040\255\
\000\255\251\254\034\255\000\000\008\255\000\255\000\255\000\000\
\000\000\034\255\034\255"

let yyrindex = "\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\255\000\000\000\000\000\000\
\000\000\000\000\000\000\051\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\255\000\000\000\000\
\050\255\000\000\000\000\000\000\000\000\000\000\000\000\249\254\
\000\000\000\000\009\255\000\000\000\000\000\000\000\000\000\000\
\000\000\013\255\014\255"

let yygindex = "\000\000\
\000\000\046\000\000\000\000\000\252\255\016\000\010\000\000\000\
\000\000\219\255"

let yytablesize = 67
let yytable = "\022\000\
\022\000\054\000\055\000\045\000\046\000\047\000\001\000\022\000\
\048\000\051\000\022\000\053\000\056\000\049\000\054\000\055\000\
\058\000\059\000\014\000\009\000\029\000\012\000\057\000\015\000\
\015\000\033\000\015\000\017\000\018\000\010\000\017\000\018\000\
\003\000\004\000\005\000\011\000\018\000\019\000\016\000\017\000\
\054\000\055\000\027\000\021\000\024\000\035\000\028\000\003\000\
\023\000\025\000\031\000\034\000\030\000\013\000\039\000\036\000\
\044\000\052\000\042\000\037\000\038\000\000\000\000\000\010\000\
\043\000\022\000\012\000"

let yycheck = "\007\001\
\008\001\007\001\008\001\004\001\005\001\006\001\001\000\015\001\
\009\001\047\000\018\001\049\000\018\001\014\001\007\001\008\001\
\054\000\055\000\012\001\009\001\025\000\000\000\015\001\015\001\
\018\001\030\000\018\001\015\001\015\001\009\001\018\001\018\001\
\001\001\002\001\003\001\014\001\004\001\005\001\014\001\010\001\
\007\001\008\001\015\001\009\001\018\001\015\001\009\001\000\000\
\013\001\013\001\011\001\009\001\014\001\008\000\009\001\015\001\
\041\000\048\000\012\001\018\001\016\001\255\255\255\255\015\001\
\017\001\015\001\017\001"

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
# 158 "pcfparse.ml"
               : Pcfast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 21 "pcfparse.mly"
               ( _1 :: _2 )
# 166 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "pcfparse.mly"
                ( [] )
# 172 "pcfparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'boolval) in
    Obj.repr(
# 26 "pcfparse.mly"
      ( InputDecl(_2, Some _4) )
# 180 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 29 "pcfparse.mly"
      ( InputDecl(_2, None) )
# 187 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'params) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 32 "pcfparse.mly"
      ( GateDecl(_2, _4, _7, _10) )
# 197 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idnext) in
    Obj.repr(
# 35 "pcfparse.mly"
      ( PrintStmt(_3, _5, _6) )
# 206 "pcfparse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 38 "pcfparse.mly"
                      ( _1 :: _2 )
# 214 "pcfparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 40 "pcfparse.mly"
                 ( _2 )
# 221 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "pcfparse.mly"
                ( [] )
# 227 "pcfparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 44 "pcfparse.mly"
               ( _1 :: _2 )
# 235 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "pcfparse.mly"
                ( [] )
# 241 "pcfparse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "pcfparse.mly"
      ( Assign(_1, _3) )
# 249 "pcfparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 52 "pcfparse.mly"
                           ( Parens(_2) )
# 256 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "pcfparse.mly"
                           ( Not(_2) )
# 263 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idnext) in
    Obj.repr(
# 54 "pcfparse.mly"
                           ( Var(_1, _2) )
# 271 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "pcfparse.mly"
                           ( And(_1, _3) )
# 279 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "pcfparse.mly"
                           ( Or(_1, _3) )
# 287 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "pcfparse.mly"
                           ( True )
# 293 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "pcfparse.mly"
                           ( False )
# 299 "pcfparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "pcfparse.mly"
                           ( Some(_2) )
# 306 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "pcfparse.mly"
                           ( None )
# 312 "pcfparse.ml"
               : 'idnext))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "pcfparse.mly"
          ( true )
# 318 "pcfparse.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "pcfparse.mly"
          ( false )
# 324 "pcfparse.ml"
               : 'boolval))
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
