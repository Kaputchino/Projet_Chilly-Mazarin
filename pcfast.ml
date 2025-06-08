(*─────────────────────────── pcfast.ml ────────────────────────────*)

(* ── 1.  Valeurs logiques ──────────────────────────────────────── *)
type signal = TTrue | TFalse | TUndet

let str_of_sig = function
  | TTrue  -> "true"
  | TFalse -> "false"
  | TUndet -> "undet"

(* ── 2.  Expressions ───────────────────────────────────────────── *)
type expr =
  | True
  | False
  | Not    of expr
  | And    of expr * expr
  | Or     of expr * expr
  | Var    of string * string option      (* x   |  ha.sum *)
  | Parens of expr

(* ── 3.  Instructions dans le corps d’une gate ─────────────────── *)
type stmt =
  | Assign     of string * expr
  | InstAssign of string           (* alias local    *)
               * string           (* nom de la gate *)
               * expr   list      (* arguments (expr) *)

(* ── 4.  Cibles du WRITE │PRINT ────────────────────────────────── *)
type target =
  | TGate   of string
  | TSignal of string * string option

(* ── 5.  Déclarations top-level ────────────────────────────────── *)
type decl =
  | InputDecl  of string * signal option
  | GateDecl   of string * string list * string list * stmt list
  | InstDecl   of string * string * string list    (* alias = gate(args…) *)
  | PrintStmt  of string * string * string option
  | WriteStmt  of string * target list

type program = decl list

(* ── 6.  Fonctions d’affichage (debug) ─────────────────────────── *)

let rec string_of_expr = function
  | True               -> "true"
  | False              -> "false"
  | Not e              -> Printf.sprintf "(!%s)" (string_of_expr e)
  | And (e1,e2)        -> Printf.sprintf "(%s - %s)" (string_of_expr e1) (string_of_expr e2)
  | Or  (e1,e2)        -> Printf.sprintf "(%s + %s)" (string_of_expr e1) (string_of_expr e2)
  | Var (id,None)      -> id
  | Var (id,Some sub)  -> id ^ "." ^ sub
  | Parens e           -> "(" ^ string_of_expr e ^ ")"

let rec print_expr oc e = output_string oc (string_of_expr e)

let print_stmt oc = function
  | Assign (v,e) ->
      Printf.fprintf oc "%s = %a;" v print_expr e
  | InstAssign (a,g,args) ->
      Printf.fprintf oc "%s = %s(%s);" a g
        (String.concat ", " (List.map string_of_expr args))

let print_target oc = function
  | TGate g                -> output_string oc g
  | TSignal (id,None)      -> output_string oc id
  | TSignal (id,Some sub)  -> output_string oc (id ^ "." ^ sub)

let rec print_decl oc = function
  | InputDecl (id,None) ->
      Printf.fprintf oc "input %s;" id
  | InputDecl (id,Some v) ->
      Printf.fprintf oc "input %s = %s;" id (str_of_sig v)
  | GateDecl (n,ins,outs,body) ->
      Printf.fprintf oc "gate %s(%s)(%s) {\n"
        n (String.concat ", " ins) (String.concat ", " outs);
      List.iter (fun s -> Printf.fprintf oc "  %a\n" print_stmt s) body;
      Printf.fprintf oc "}"
  | InstDecl (a,g,actuals) ->
      Printf.fprintf oc "%s = %s(%s);" a g (String.concat ", " actuals)
  | PrintStmt (msg,id,None) ->
      Printf.fprintf oc "print(\"%s\", %s);" msg id
  | PrintStmt (msg,id,Some sub) ->
      Printf.fprintf oc "print(\"%s\", %s.%s);" msg id sub
  | WriteStmt (path,targets) ->
      Printf.fprintf oc "write(\"%s\"" path;
      List.iter (fun t -> Printf.fprintf oc ", %a" print_target t) targets;
      Printf.fprintf oc ");"


  let print_program oc prog =
  List.iter
    (fun d ->
       print_decl oc d;
       Printf.fprintf oc "\n")
    prog
