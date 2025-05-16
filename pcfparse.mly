%{
open Pcfast ;;

let rec mkfun params expr =
  match params with
  | [] -> expr
  | p :: prms -> EFun(p, mkfun prms expr)
;;
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS MULT DIV EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%token LPAR RPAR SEMISEMI
%token LET REC IN FUN ARROW
%token IF THEN ELSE
%left EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start main
%type <Pcfast.expr> main

%%

main: expr SEMISEMI { $1 }
    | SEMISEMI main { $2 }
;

/* Grammaire */

expr:
  LET REC IDENT IDENT seqident EQUAL expr IN expr
         { ELetrec ($3, $4, (mkfun $5 $7), $9) }
| LET IDENT seqident EQUAL expr IN expr
         { ELet ($2, (mkfun $3 $5) , $7) }
| FUN IDENT ARROW expr
         { EFun ($2, $4) }
| IF expr THEN expr ELSE expr
         { EIf ($2, $4, $6) }
| arith_expr
         { $1 }
;

seqident:
  IDENT seqident  { $1 :: $2 }
| /* rien */      { [] }
;

arith_expr:
  arith_expr EQUAL arith_expr        { EBinop ("=", $1, $3) }
| arith_expr GREATER arith_expr      { EBinop (">", $1, $3) }
| arith_expr GREATEREQUAL arith_expr { EBinop (">=", $1, $3) }
| arith_expr SMALLER arith_expr      { EBinop ("<", $1, $3) }
| arith_expr SMALLEREQUAL arith_expr { EBinop ("<=", $1, $3) }
| arith_expr PLUS arith_expr         { EBinop ("+", $1, $3) }
| arith_expr MINUS arith_expr        { EBinop ("-", $1, $3) }
| arith_expr MULT arith_expr         { EBinop ("*", $1, $3) }
| arith_expr DIV arith_expr          { EBinop ("/", $1, $3) }
| application                        { $1 }
;

/* On considere ci-dessous que MINUS atom est dans la categorie
 * des applications. Cela permet de traiter n - 1
 * comme une soustraction binaire, et       f (- 1)
 * comme l'application de f a l'oppose de 1.
 */

application:
  application atom { EApp ($1, $2) }
| MINUS atom       { EMonop ("-", $2) }
| atom             { $1 }
;

atom:
  INT            { EInt ($1) }
| TRUE           { EBool (true) }
| FALSE          { EBool (false) }
| STRING         { EString ($1) }
| IDENT          { EIdent ($1) }
| LPAR expr RPAR { $2 }
;
