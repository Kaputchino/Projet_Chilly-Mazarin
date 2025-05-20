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

let string_of_bool = function true -> "true" | false -> "false"

let keys tbl =
  Hashtbl.fold (fun k _ acc -> k :: acc) tbl []

  
let find tbl name kind =
  try Hashtbl.find tbl name
  with Not_found ->
    error (Printf.sprintf "⟨%s⟩ \"%s\" introuvable" kind name)




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




let read_header_if_exists path =
  if Sys.file_exists path then
    let ic = open_in path in
    let line = input_line ic in
    close_in ic;
    String.split_on_char ',' line
  else []

let ensure_columns ctx targets header =
  let cols = ref header in
  let add_col c = if not (List.mem c !cols) then cols := !cols @ [c] in
  List.iter (function
    | TGate id ->
        (* exécute d’abord  (instance ou gate) *)
        if Hashtbl.mem ctx.instances id then
          exec_instance ctx (Hashtbl.find ctx.instances id)
        else if Hashtbl.mem ctx.gates id then
          exec_gate ctx id;

        (* ajoute toutes ses sorties (alias.prefix) *)
        let add_outputs alias gdef =
          List.iter (fun o -> add_col (alias ^ "." ^ o)) gdef.outputs
        in
        if Hashtbl.mem ctx.instances id then
          let inst = Hashtbl.find ctx.instances id in
          add_outputs inst.alias inst.gdef
        else if Hashtbl.mem ctx.gates id then
          let gdef = Hashtbl.find ctx.gates id in
          add_outputs id gdef;

        (* puis ajoute éventuels internes déjà présents *)
        let prefix = id ^ "." in
        Hashtbl.iter
          (fun k _ -> if String.length k >= String.length prefix
                      && String.sub k 0 (String.length prefix) = prefix
                      then add_col k)
          ctx.signals

    | TSignal (id, opt) ->
      let col = match opt with None -> id | Some f -> id ^ "." ^ f in
      add_col col

  ) targets;
  !cols
let row_of_columns ctx cols =
  cols
  |> List.map (fun c ->
        if Hashtbl.find ctx.signals c then "TRUE" else "FALSE")
  |> String.concat ","

let write_csv path header row =
  let oc = open_out path in
  output_string oc (String.concat "," header ^ "\n" ^ row ^ "\n");
  close_out oc


      (* ──────────── définition mutuelle ──────────── *)

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
    | WriteStmt (path, targets) ->
        (* 1 : exécuter chaque cible gate/instance pour remplir ctx.signals *)
        List.iter (function
          | TGate id when Hashtbl.mem ctx.gates id ->
              exec_gate ctx id
          | TGate id when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | TSignal (id,_) when Hashtbl.mem ctx.gates id ->
              exec_gate ctx id
          | TSignal (id,_) when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | _ -> ()
        ) targets;

        (* 2 : entête existant *)
        let header0 = read_header_if_exists path in

        (* 3 : nouvelles colonnes complètes *)
        let header  = ensure_columns ctx targets header0 in

        (* 4 : ligne de valeurs *)
        let row     = row_of_columns ctx header in

        (* 5 : écriture/écrasement *)
        write_csv path header row;
        Printf.printf "→ CSV mis à jour : %s\n" path
    | _ -> ()
  ) prog;

  ctx
