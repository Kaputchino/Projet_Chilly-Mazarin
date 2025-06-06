%{
open Pcfast
%}

%token INPUT GATE PRINT TRUE FALSE
%token NOT AND OR
%token <string> IDENT
%token <string> STRING
%token DOT EQUAL COMMA LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF WRITE FILL

%start main
%type <Pcfast.program> main

%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | /* empty */ { [] }

/* ---------- WRITE <chemin, cibles…> ------------------------------ */
wtargets:
  | wtarget more_wtargets { $1 :: $2 }
more_wtargets:
  | COMMA wtargets { $2 }
  | /* empty */    { [] }

wtarget:
  | IDENT DOT IDENT { TSignal($1, Some $3) }
  | IDENT           { TSignal($1, None) }

/* ---------- littéraux logiques pour les entrées ------------------ */
boolval:
  | TRUE  { TTrue }
  | FALSE { TFalse }

/* ---------- déclarations top-level ------------------------------- */
decl:
  | INPUT IDENT EQUAL boolval SEMICOLON
      { InputDecl($2, Some $4) }
  | INPUT IDENT SEMICOLON
      { InputDecl($2, None) }

  | GATE IDENT LPAREN params RPAREN LPAREN params RPAREN LBRACE stmts RBRACE
      { GateDecl($2, $4, $7, $10) }

  | IDENT EQUAL IDENT LPAREN args RPAREN SEMICOLON
      { InstDecl($1, $3, $5) }

  | PRINT LPAREN STRING COMMA IDENT idnext RPAREN SEMICOLON
      { PrintStmt($3, $5, $6) }

  | WRITE LPAREN STRING COMMA wtargets RPAREN SEMICOLON
      { WriteStmt($3, $5) }

/* ---------- listes de paramètres -------------------------------- */
params:
  | IDENT more_params { $1 :: $2 }
more_params:
  | COMMA params { $2 }
  | /* empty */ { [] }

/* ---------- corps de gate --------------------------------------- */
stmts:
  | stmt stmts { $1 :: $2 }
  | /* empty */ { [] }

stmt:
  | IDENT EQUAL expr SEMICOLON { Assign($1, $3) }

/* ---------- expressions ----------------------------------------- */
expr:
  | LPAREN expr RPAREN     { Parens($2) }
  | NOT expr               { Not($2) }
  | IDENT idnext           { Var($1, $2) }
  | expr AND expr          { And($1, $3) }
  | expr OR expr           { Or($1, $3) }
  | TRUE                   { True }
  | FALSE                  { False }

idnext:
  | DOT IDENT { Some($2) }
  | /* empty */ { None }

/* ---------- appel de gate (arguments) --------------------------- */
args:
  | IDENT more_args { $1 :: $2 }
more_args:
  | COMMA args { $2 }
  | /* empty */ { [] }
