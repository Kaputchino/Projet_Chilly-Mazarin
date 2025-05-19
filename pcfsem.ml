open Pcfast

type signal = bool 
type context = {
  inputs : (string, signal) Hashtbl.t;
  signals : (string, signal) Hashtbl.t;
  gates : (string, gatedef) Hashtbl.t;
  instances : (string, instance) Hashtbl.t;

}
and gatedef = {
  name : string;
  inputs : string list;
  outputs : string list;
  body : stmt list;
}
and instance = {
  alias   : string;
  gdef    : gatedef;
  actuals : string list;
}



let error msg = raise (Failure msg)

let find tbl name kind =
  try Hashtbl.find tbl name
  with Not_found ->
    error (Printf.sprintf "⟨%s⟩ \"%s\" introuvable" kind name)



      (* ──────────── définition mutuelle ──────────── *)
let rec exec_gate ctx gate_name =
  (* récupère la définition *)
  let gate =
    try Hashtbl.find ctx.gates gate_name
    with Not_found -> error ("Gate non trouvée : " ^ gate_name)
  in
  (* contexte local pour exécuter la porte *)
  let local_ctx = { ctx with signals = ctx.signals } in
  List.iter (fun id ->
    let v = find ctx.signals id ("entrée de gate "^gate.name)
    in Hashtbl.add local_ctx.signals id v
  ) gate.inputs;
  (* exécution des lignes *)
  List.iter (function
      | Assign (id, e) ->
          let value = eval_expr local_ctx e in      (* ← appelle eval_expr *)
          Hashtbl.replace local_ctx.signals id value
  ) gate.body;
  (* recopie des sorties dans les signaux globaux *)
  List.iter (fun o ->
    let v = find local_ctx.signals o ("sortie "^o^" de "^gate.name)
    in Hashtbl.replace ctx.signals (gate.name ^ "." ^ o) v
  ) gate.outputs

and eval_expr ctx expr =
  let get_var name =
    if Hashtbl.mem ctx.signals name then find ctx.signals name "signal"
    else if Hashtbl.mem ctx.inputs  name then find ctx.inputs  name "input"
    else error (Printf.sprintf "Nom \"%s\" non défini" name)
  in
  match expr with
  | True  -> true
  | False -> false
  | Not e -> not (eval_expr ctx e)
  | And (a,b) -> eval_expr ctx a && eval_expr ctx b
  | Or  (a,b) -> eval_expr ctx a || eval_expr ctx b
  | Parens e  -> eval_expr ctx e
  | Var (id,None) ->
      if Hashtbl.mem ctx.signals id then get_var id
      else if Hashtbl.mem ctx.gates id then (
        exec_gate ctx id;                    (* calcule la gate si besoin *)
        get_var id )
      else error ("Signal non défini : " ^ id)
  | Var (id,Some sub) ->
      let full = id ^ "." ^ sub in
      if not (Hashtbl.mem ctx.signals full) && Hashtbl.mem ctx.gates id
      then exec_gate ctx id;                (* calcule la gate id *)
      get_var full
(* ────────────────────────────────────────────── *)
let exec_instance ctx inst =
  (* lier paramètres formels -> signaux réels *)
  List.iter2 (fun formal actual ->
    let v = find ctx.signals actual "signal" in
    Hashtbl.replace ctx.signals formal v
  ) inst.gdef.inputs inst.actuals;
  exec_gate ctx inst.gdef.name;
  (* recopier les sorties sous le préfixe alias *)
  List.iter (fun o ->
    let v = Hashtbl.find ctx.signals (inst.gdef.name ^ "." ^ o) in
    Hashtbl.replace ctx.signals (inst.alias ^ "." ^ o) v
  ) inst.gdef.outputs

(* Exécution d’un programme complet *)
let run_program prog =
  let ctx = {
    inputs = Hashtbl.create 10;
    signals = Hashtbl.create 100;
    gates = Hashtbl.create 50;
    instances = Hashtbl.create 50;
  } in

  (* Déclaration des entrées et des gates *)
  List.iter (function
    | InputDecl (id, opt_val) ->
        let value = match opt_val with Some v -> v | None -> false in
        Hashtbl.add ctx.inputs id value;
        Hashtbl.add ctx.signals id value
    | GateDecl (name, ins, outs, body) ->
        Hashtbl.add ctx.gates name { name; inputs = ins; outputs = outs; body }
    | InstDecl (alias, gname, actuals) ->
      let gdef =
        try Hashtbl.find ctx.gates gname
        with Not_found -> error ("Gate "^gname^" inconnue")
      in
      Hashtbl.add ctx.instances alias { alias; gdef; actuals }
    | _ -> ()
  ) prog;
      Hashtbl.iter (fun _ inst -> exec_instance ctx inst) ctx.instances;

      (*auto print*)
  Hashtbl.iter
    (fun id v -> Printf.printf "input %s = %b\n" id v)
    ctx.inputs;


  (* Exécution des print avec calcul si besoin *)
  List.iter (function
    | PrintStmt (msg, id, None) when Hashtbl.mem ctx.inputs id ->
        let v = Hashtbl.find ctx.inputs id in
        Printf.printf "%s %s = %b\n" msg id v
    | PrintStmt (msg, id, opt) ->
        let full =
          match opt with
          | None -> id
          | Some sub ->
              let gate = id in
              if Hashtbl.mem ctx.gates gate then exec_gate ctx gate;
              gate ^ "." ^ sub
        in
        let value = find ctx.signals full "print"
        in
        Printf.printf "%s %s = %b\n" msg full value
        
    | _ -> ()
  ) prog;

  ctx
