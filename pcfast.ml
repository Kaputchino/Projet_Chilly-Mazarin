type signal = TTrue | TFalse | TUndet
type expr =
  | True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Var of string * string option
  | Parens of expr

type stmt =
  | Assign of string * expr

type target =
  | TGate   of string                   (* ha, fa … *)
  | TSignal of string * string option   (* ha.sum / ha / a  *)

type decl =         
  | InputDecl of string * signal option
  | GateDecl  of string * string list * string list * stmt list
  | InstDecl  of string * string * string list
  | PrintStmt of string * string * string option
  | WriteStmt of string * target list
                
  

type program = decl list

let str_of_sig = function                 (* 3 *)
  | TTrue  -> "true"
  | TFalse -> "false"
  | TUndet -> "undet"

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
  | Assign (v, e) -> Printf.fprintf oc "%s = %a;" v print_expr e

let print_decl oc = function
  | InputDecl (id, None) ->                              (* 4 *)
      Printf.fprintf oc "input %s;" id
  | InputDecl (id, Some v) ->
      Printf.fprintf oc "input %s = %s;" id (str_of_sig v)

  | InstDecl (alias, gname, actuals) ->
      Printf.fprintf oc "%s = %s(%s);" alias gname (String.concat ", " actuals)
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
