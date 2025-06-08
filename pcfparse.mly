/*──────────────────────── pcfparse.mly ───────────────────────*/
%{
open Pcfast
%}

%token INPUT GATE PRINT WRITE
%token TRUE FALSE UNDET
%token NOT AND OR
%token <string> IDENT
%token <string> STRING
%token DOT EQUAL COMMA LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF

%start main
%type <Pcfast.program> main
%%

main: decls EOF { $1 }

/*────────────────── déclarations top-level ──────────────────*/
decls:
  | decl decls { $1 :: $2 }
  |            { [] }

decl:
  /* inputs -------------------------------------------------- */
  | INPUT IDENT SEMICOLON
      { InputDecl($2, None) }
  | INPUT IDENT EQUAL boollit SEMICOLON
      { InputDecl($2, Some $4) }

  /* gate definition ---------------------------------------- */
  | GATE IDENT LPAREN idlist RPAREN LPAREN idlist RPAREN
          LBRACE stmts RBRACE
      { GateDecl($2, $4, $7, $10) }

  /* global instantiation ----------------------------------- */
  | IDENT EQUAL IDENT LPAREN callargs RPAREN SEMICOLON
      { let ids =
          List.map (function
              | Var(id,None)     -> id
              | Var(id,Some sub) -> id ^ "." ^ sub
              | _ -> failwith "global InstDecl: args simples") $5
        in InstDecl($1,$3,ids) }

  /* print / write ------------------------------------------ */
  | PRINT LPAREN STRING COMMA ident_or_gate RPAREN SEMICOLON
      { let (id,sub) = $5 in PrintStmt($3,id,sub) }

  | WRITE LPAREN STRING COMMA wtargets RPAREN SEMICOLON
      { WriteStmt($3,$5) }

/*──────────────── lists & helpers ───────────────────────────*/
idlist:
  | IDENT more_ids { $1 :: $2 }
more_ids:
  | COMMA idlist { $2 } | { [] }

wtargets:
  | wtarget more_wt { $1 :: $2 }
more_wt:
  | COMMA wtargets { $2 } | { [] }
wtarget:
  | IDENT DOT IDENT { TSignal($1,Some $3) }
  | IDENT           { TSignal($1,None) }

ident_or_gate:
  | IDENT DOT IDENT { ($1,Some $3) }
  | IDENT           { ($1,None) }

boollit:
  | TRUE  { TTrue } | FALSE { TFalse } | UNDET { TUndet }

/*─────────────────── statements inside gate ────────────────*/
stmts:
  | stmt stmts { $1 :: $2 } | { [] }

stmt:
  | IDENT EQUAL expr SEMICOLON
      { Assign($1,$3) }
  | IDENT EQUAL IDENT LPAREN callargs RPAREN SEMICOLON
      { InstAssign($1,$3,$5) }

/*────────────── call-time argument list (expr list) ─────────*/
callargs:
  | expr more_cargs { $1 :: $2 }
more_cargs:
  | COMMA callargs { $2 } | { [] }

/*──────────────────── expressions ───────────────────────────*/
expr:
  | LPAREN expr RPAREN     { Parens($2) }
  | NOT expr               { Not($2) }
  | expr AND expr          { And($1,$3) }
  | expr OR  expr          { Or ($1,$3) }
  | IDENT after_ident      { Var($1,$2) }
  | TRUE                   { True }
  | FALSE                  { False }

after_ident:
  | DOT IDENT { Some $2 } | { None }
