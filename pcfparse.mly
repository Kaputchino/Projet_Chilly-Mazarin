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
    decls EOF                 { $1 }

decls:
    decl decls                { $1 :: $2 }
  | /* empty */               { [] }

/* ---------- write ----------- */
wtargets:
    wtarget more_wtargets     { $1 :: $2 }
more_wtargets:
    COMMA wtargets            { $2 }
  | /* empty */               { [] }

wtarget:
    IDENT DOT IDENT           { TSignal($1, Some $3) }
  | IDENT                     { TSignal($1, None) }

/* ---------- bool lit -------- */
boolval:
    TRUE                      { true }
  | FALSE                     { false }

/* ---------- decl ------------ */
decl:
    INPUT IDENT EQUAL boolval SEMICOLON
                               { InputDecl($2, Some $4) }
  | INPUT IDENT SEMICOLON      { InputDecl($2, None) }
  | GATE IDENT LPAREN params RPAREN LPAREN params RPAREN
         LBRACE stmts RBRACE   { GateDecl($2,$4,$7,$10) }
  | PRINT LPAREN STRING COMMA IDENT idnext RPAREN SEMICOLON
                               { PrintStmt($3,$5,$6) }
  | IDENT EQUAL IDENT LPAREN callargs RPAREN SEMICOLON
                               { InstDecl
     ( $1,                        (* alias            *)
       $3,                        (* nom de la gate   *)
       List.map                   (* convertir expr ↦ ident *)
         (function
            | Var(id,None) -> id
            | _ -> failwith "arguments must be simples idents")
         $5 ) }

  | WRITE LPAREN STRING COMMA wtargets RPAREN SEMICOLON
                               { WriteStmt($3,$5) }

/* ---------- params ---------- */
params:
    IDENT more_params         { $1 :: $2 }
more_params:
    COMMA params              { $2 }
  | /* empty */               { [] }

/* ---------- stmts ----------- */
stmts:
    stmt stmts                { $1 :: $2 }
  | /* empty */               { [] }

stmt:
    IDENT EQUAL expr SEMICOLON
                                { Assign($1,$3) }
  | IDENT EQUAL IDENT LPAREN callargs RPAREN SEMICOLON
    { InstAssign ($1, $3, $5) }

/* ---------- call args ------- */
callargs:
    expr more_cargs           { $1 :: $2 }
more_cargs:
    COMMA callargs            { $2 }
  | /* empty */               { [] }

/* ---------- expressions ----- */
expr:
    LPAREN expr RPAREN        { Parens($2) }
  | NOT expr                  { Not($2) }
  | IDENT idnext              { Var($1,$2) }
  | expr AND expr             { And($1,$3) }
  | expr OR  expr             { Or($1,$3) }
  | TRUE                      { True }
  | FALSE                     { False }

/* ----- ident.suffix --------- */
idnext:
    DOT IDENT                 { Some($2) }
  | /* empty */               { None }
