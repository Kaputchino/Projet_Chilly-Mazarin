type expr =
  | True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Var of string * string option
  | Parens of expr

type stmt =
  | Assign     of string * expr
  | InstAssign of string * string * expr list  (* alias, gate, args *)
  

type target =
  | TGate   of string                   (* ha, fa … *)
  | TSignal of string * string option   (* ha.sum / ha / a  *)
  

type decl =
  | InputDecl  of string * bool option
  | GateDecl   of string * string list * string list * stmt list
  | InstDecl   of string            (* alias, ex. g *)
                * string            (* nom de la gate, ex. halfadder *)
                * string list       (* arguments, ex. [a; super] *)
  | PrintStmt  of string * string * string option
  | WriteStmt of string * target list   (* chemin + liste de cibles *)
                
  

type program = decl list

let rec string_of_expr = function
  | True               -> "true"
  | False              -> "false"
  | Not e              -> Printf.sprintf "(!%s)"   (string_of_expr e)
  | And (e1,e2)        -> Printf.sprintf "(%s - %s)" (string_of_expr e1) (string_of_expr e2)
  | Or  (e1,e2)        -> Printf.sprintf "(%s + %s)" (string_of_expr e1) (string_of_expr e2)
  | Var (id,None)      -> id
  | Var (id,Some sub)  -> Printf.sprintf "%s.%s" id sub
  | Parens e           -> Printf.sprintf "(%s)"    (string_of_expr e)



let rec print_expr oc = function
  | True -> output_string oc "true"
  | False -> output_string oc "false"
  | Not e -> Printf.fprintf oc "(!%a)" print_expr e
  | And (e1, e2) -> Printf.fprintf oc "(%a - %a)" print_expr e1 print_expr e2
  | Or (e1, e2) -> Printf.fprintf oc "(%a + %a)" print_expr e1 print_expr e2
  | Var (id, None) -> output_string oc id
  | Var (id, Some sub) -> Printf.fprintf oc "%s.%s" id sub
  | Parens e -> Printf.fprintf oc "(%a)" print_expr e

let print_stmt oc = function
  | Assign (v,e) ->
      Printf.fprintf oc "%s = %a;" v print_expr e
  | InstAssign (a, g, args) ->
      Printf.fprintf oc "%s = %s(%s);" a g
        (String.concat ", " (List.map string_of_expr args))


let print_decl oc = function
  | InputDecl (id, None) ->
      (* entrée sans valeur explicite *)
      Printf.fprintf oc "input %s;" id
  | InstDecl (alias, gname, actuals) ->
     Printf.fprintf oc "%s = %s(%s);" alias gname (String.concat ", " actuals)
  | InputDecl (id, Some v) ->
      (* entrée avec valeur true / false *)
      Printf.fprintf oc "input %s = %s;" id (if v then "true" else "false")
  | GateDecl (name, ins, outs, stmts) ->
      Printf.fprintf oc "gate %s(%s)(%s) {\n" name
        (String.concat ", " ins)
        (String.concat ", " outs);
      List.iter (fun s -> Printf.fprintf oc "  %a\n" print_stmt s) stmts;
      Printf.fprintf oc "}"
  | WriteStmt (path, targets) ->
      let pp_t oc = function
        | TGate g                 -> Printf.fprintf oc "%s" g
        | TSignal (id, None)      -> Printf.fprintf oc "%s" id
        | TSignal (id, Some sub)  -> Printf.fprintf oc "%s.%s" id sub
      in
      Printf.fprintf oc "write(\"%s\"" path;
      List.iter (fun t -> Printf.fprintf oc ", %a" pp_t t) targets;
      Printf.fprintf oc ");"

  | PrintStmt (msg, id, sub) ->
      match sub with
      | None -> Printf.fprintf oc "print(\"%s\", %s);" msg id
      | Some f -> Printf.fprintf oc "print(\"%s\", %s.%s);" msg id f



let print_program oc prog =
  List.iter (fun d -> print_decl oc d; Printf.fprintf oc "\n") prog
