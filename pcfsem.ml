open Pcfast

type signal = bool

type context = {
  inputs : (string, signal) Hashtbl.t;
  signals : (string, signal) Hashtbl.t;
  gates : (string, gatedef) Hashtbl.t;
}

and gatedef = {
  name : string;
  inputs : string list;
  outputs : string list;
  body : stmt list;
}

let error msg = raise (Failure msg)

(* Évaluation d'expressions logiques *)
let rec eval_expr ctx expr =
  let get_var name =
    try Hashtbl.find ctx.signals name
    with Not_found ->
      try Hashtbl.find ctx.inputs name
      with Not_found -> error ("Signal non défini : " ^ name)
  in
  match expr with
  | True -> true
  | False -> false
  | Not e -> not (eval_expr ctx e)
  | And (a, b) -> (eval_expr ctx a) && (eval_expr ctx b)
  | Or (a, b) -> (eval_expr ctx a) || (eval_expr ctx b)
  | Parens e -> eval_expr ctx e
  | Var (id, None) -> get_var id
  | Var (id, Some sub) ->
      let full = id ^ "." ^ sub in
      get_var full

(* Évaluation d'une instruction *)
let eval_stmt ctx stmt =
  match stmt with
  | Assign (id, expr) ->
      let value = eval_expr ctx expr in
      Hashtbl.replace ctx.signals id value

(* Exécution d'une gate *)
let exec_gate ctx gate_name =
  let gate =
    try Hashtbl.find ctx.gates gate_name
    with Not_found -> error ("Gate non trouvée : " ^ gate_name)
  in
  let local_ctx = {
    ctx with signals = Hashtbl.create 10;
  } in
  List.iter (fun id ->
    let v = Hashtbl.find ctx.signals id in
    Hashtbl.add local_ctx.signals id v
  ) gate.inputs;
  List.iter (eval_stmt local_ctx) gate.body;
  List.iter (fun id ->
    let v = Hashtbl.find local_ctx.signals id in
    Hashtbl.replace ctx.signals (gate.name ^ "." ^ id) v
  ) gate.outputs

(* Exécution d’un programme complet *)
let run_program prog =
  let ctx = {
    inputs = Hashtbl.create 10;
    signals = Hashtbl.create 100;
    gates = Hashtbl.create 50;
  } in

  (* Déclaration des entrées et des gates *)
  List.iter (function
    | InputDecl (id, opt_val) ->
        let value = match opt_val with Some v -> v | None -> false in
        Hashtbl.add ctx.inputs id value;
        Hashtbl.add ctx.signals id value
    | GateDecl (name, ins, outs, body) ->
        Hashtbl.add ctx.gates name { name; inputs = ins; outputs = outs; body }
    | _ -> ()
  ) prog;

  (* Exécution des print avec calcul si besoin *)
  List.iter (function
    | PrintStmt (msg, id, opt) ->
        let full =
          match opt with
          | None -> id
          | Some sub ->
              let gate = id in
              if Hashtbl.mem ctx.gates gate then exec_gate ctx gate;
              gate ^ "." ^ sub
        in
        let value =
          try Hashtbl.find ctx.signals full
          with Not_found -> error ("Signal introuvable : " ^ full)
        in
        Printf.printf "%s %s = %b\n" msg full value
    | _ -> ()
  ) prog;

  ctx
