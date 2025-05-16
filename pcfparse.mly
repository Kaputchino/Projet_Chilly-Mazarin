%{
open Pcfast
%}

%token INPUT GATE PRINT TRUE FALSE
%token NOT AND OR
%token <string> IDENT
%token <string> STRING
%token DOT EQUAL COMMA LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF

%start main
%type <program> main

%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | /* empty */ { [] }

decl:
  | INPUT IDENT SEMICOLON
      { InputDecl($2) }

  | GATE IDENT LPAREN params RPAREN LPAREN params RPAREN LBRACE stmts RBRACE
      { GateDecl($2, $4, $7, $10) }

  | PRINT LPAREN STRING COMMA IDENT idnext RPAREN SEMICOLON
      { PrintStmt($3, $5, $6) }

params:
  | IDENT more_params { $1 :: $2 }

more_params:
  | COMMA params { $2 }
  | /* empty */   { [] }

stmts:
  | stmt stmts { $1 :: $2 }
  | /* empty */ { [] }

stmt:
  | IDENT EQUAL expr SEMICOLON
      { Assign($1, $3) }

expr:
  | LPAREN expr RPAREN     { Parens($2) }
  | NOT expr               { Not($2) }
  | IDENT idnext           { Var($1, $2) }
  | expr AND expr          { And($1, $3) }
  | expr OR expr           { Or($1, $3) }
  | TRUE                   { True }
  | FALSE                  { False }

idnext:
  | DOT IDENT              { Some($2) }
  | /* empty */            { None }
