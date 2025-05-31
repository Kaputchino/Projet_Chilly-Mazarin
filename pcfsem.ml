open Pcfast

type signal = bool

type context = {
  inputs    : (string, signal) Hashtbl.t;
  signals   : (string, signal) Hashtbl.t;
  gates     : (string, gatedef) Hashtbl.t;
  instances : (string, instance) Hashtbl.t;
}
and gatedef = {
  name    : string;
  inputs  : string list;
  outputs : string list;
  body    : stmt list;
}
and instance = {
  alias   : string;
  gdef    : gatedef;
  actuals : string list;
}

let error msg = raise (Failure msg)

let string_of_bool = function true -> "true" | false -> "false"

let find tbl name kind =
  try Hashtbl.find tbl name
  with Not_found ->
    error (Printf.sprintf "⟨%s⟩ \"%s\" introuvable" kind name)

let write_csv path header row =
  let oc = open_out path in
  output_string oc (String.concat "," header);
  output_char oc '\n';
  output_string oc row;
  output_char oc '\n';
  close_out oc

(* Exécution d’une porte par son nom (sans arguments explicites), 
   en considérant que chaque paramètre formel est déjà lié dans ctx.signals. *)
let rec exec_gate_by_name ctx gate_name =
  let gdef =
    try Hashtbl.find ctx.gates gate_name
    with Not_found -> error ("Gate "^gate_name^" inconnue")
  in
  (* On forme une liste d’expressions [Var(x1,None); Var(x2,None); …] *)
  let actuals = List.map (fun id -> Var(id, None)) gdef.inputs in
  exec_gate ctx gdef actuals

(* Exécution d’une gate à partir de sa définition et d’une liste
   d’arguments (expressions). On crée un contexte local pour l’évaluation. *)
and exec_gate ctx gdef actuals =
  (* 1. Contexte local (copie des tables pour ne pas écraser le global) *)
  let local_ctx = {
    ctx with
      signals   = Hashtbl.copy ctx.signals;
      instances = Hashtbl.copy ctx.instances;
  } in
  (* 2. Liaison des arguments (on évalue chaque expr pour avoir un bool) *)
  List.iter2
    (fun formal actual_expr ->
       let value = eval_expr ctx actual_expr in
       Hashtbl.replace local_ctx.signals formal value)
    gdef.inputs
    actuals;
  (* 3. Exécution du corps : chaque Assign ou InstAssign modifie local_ctx.signals *)
  List.iter (fun s -> eval_stmt local_ctx s) gdef.body;
  (* 4. Recopie des sorties calculées vers le contexte global (ctx.signals) *)
  List.iter
    (fun o ->
       let v = find local_ctx.signals o ("sortie "^o^" de "^gdef.name) in
       Hashtbl.replace ctx.signals (gdef.name ^ "." ^ o) v
    )
    gdef.outputs

and eval_expr ctx = function
  | True  -> true
  | False -> false
  | Not e -> not (eval_expr ctx e)
  | And (a, b) -> (eval_expr ctx a) && (eval_expr ctx b)
  | Or  (a, b) -> (eval_expr ctx a) || (eval_expr ctx b)
  | Parens e   -> eval_expr ctx e
  | Var (id, None) ->
      if Hashtbl.mem ctx.signals id then find ctx.signals id "signal"
      else if Hashtbl.mem ctx.gates id then (
        (* Evaluation d’une gate “sans sous-champ” : ex. “halfadder” tout seul *)
        exec_gate_by_name ctx id;
        find ctx.signals id "signal"
      ) else
        error ("Signal non défini : " ^ id)
  | Var (id, Some sub) ->
      let full = id ^ "." ^ sub in
      (* Si c’est une instance et qu’elle n’a pas déjà été exécutée -> on l’exécute *)
      (match Hashtbl.find_opt ctx.instances id with
       | Some inst when not (Hashtbl.mem ctx.signals full) ->
           exec_instance ctx inst
       | _ -> ());
      find ctx.signals full "signal"

and eval_stmt ctx = function
  | Assign (id, e) ->
      let value = eval_expr ctx e in
      Hashtbl.replace ctx.signals id value

  | InstAssign (alias, gname, args_expr) ->
      (* 1. Récupérer la définition de la gate “gname” *)
      let gdef =
        match Hashtbl.find_opt ctx.gates gname with
        | Some g -> g
        | None   -> error ("Gate "^gname^" inconnue")
      in
      (* 2. Vérifier l’arité *)
      if List.length args_expr <> List.length gdef.inputs then
        error (Printf.sprintf "Arity mismatch : %s attend %d args"
                 gname (List.length gdef.inputs));
      (* 3. Transformer chaque expr en ident simple ou “a.b” pour enregistrer l’instance *)
      let args_ids =
        List.map
          (function
            | Var (x, None)     -> x
            | Var (x, Some sub) -> x ^ "." ^ sub
            | _ -> error "InstAssign : arguments doivent être Var(x, opt_sub)")
          args_expr
      in
      (* 4. Enregistrer l’instance dans le contexte global *)
      Hashtbl.replace ctx.instances alias { alias; gdef; actuals = args_ids };
      (* 5. Exécuter immédiatement la gate pour cette instance *)
      exec_gate ctx gdef args_expr;
      (* 6. Recopier chaque sortie sous “alias.sortie” *)
      List.iter
        (fun o ->
           let v = find ctx.signals (gdef.name ^ "." ^ o) "sortie" in
           Hashtbl.replace ctx.signals (alias ^ "." ^ o) v
        )
        gdef.outputs

and exec_instance ctx inst =
  (* 1. Lier chaque paramètre formel au signal réel dans ctx.signals *)
  List.iter2
    (fun formal actual ->
       let v = find ctx.signals actual "signal" in
       Hashtbl.replace ctx.signals formal v)
    inst.gdef.inputs
    inst.actuals;
  (* 2. Créer la liste d’expr Var(...) et invoquer exec_gate *)
  let actuals = List.map (fun id -> Var(id, None)) inst.actuals in
  exec_gate ctx inst.gdef actuals;
  (* 3. Recopier les sorties sous “alias.sortie” *)
  let pref_in  = inst.gdef.name ^ "." in
  let len_in   = String.length pref_in in
  let pref_out = inst.alias ^ "." in
  Hashtbl.iter
    (fun k v ->
       if String.length k >= len_in &&
          String.sub k 0 len_in = pref_in then
         let suffix = String.sub k len_in (String.length k - len_in) in
         Hashtbl.replace ctx.signals (pref_out ^ suffix) v
    )
    ctx.signals

let read_header_if_exists path =
  if Sys.file_exists path then
    let ic = open_in path in
    let line = input_line ic in
    close_in ic;
    String.split_on_char ',' line
  else
    []

let ensure_columns ctx targets header0 =
  let cols = ref header0 in
  let add_col c =
    if not (List.mem c !cols) then cols := !cols @ [c]
  in
  (* 1. Toujours commencer par les inputs *)
  Hashtbl.iter (fun k _ -> add_col k) ctx.inputs;
  (* 2. Fonction auxiliaire pour ajouter toutes les sorties d’une gate/instance *)
  let add_outputs alias gdef =
    List.iter (fun o -> add_col (alias ^ "." ^ o)) gdef.outputs
  in
  (* 3. Pour chaque cible demandée, on complète la liste des colonnes *)
  List.iter
    (function
      | TGate id ->
          if Hashtbl.mem ctx.instances id then
            exec_instance ctx (Hashtbl.find ctx.instances id)
          else if Hashtbl.mem ctx.gates id then
            exec_gate_by_name ctx id;
          (* Ajouter les sorties officielles *)
          (match Hashtbl.find_opt ctx.instances id with
           | Some inst -> add_outputs inst.alias inst.gdef
           | None ->
               (match Hashtbl.find_opt ctx.gates id with
                | Some gdef -> add_outputs id gdef
                | None -> ()));
          (* Ajouter tous les signaux internes existants “alias.” *)
          let prefix = id ^ "." in
          Hashtbl.iter
            (fun k _ ->
               if String.length k >= String.length prefix &&
                  String.sub k 0 (String.length prefix) = prefix then
                 add_col k)
            ctx.signals

      | TSignal (id, Some sub) ->
          add_col (id ^ "." ^ sub)

      | TSignal (id, None) ->
          if Hashtbl.mem ctx.instances id || Hashtbl.mem ctx.gates id then begin
            if Hashtbl.mem ctx.instances id then
              exec_instance ctx (Hashtbl.find ctx.instances id)
            else
              exec_gate_by_name ctx id;
            (match Hashtbl.find_opt ctx.instances id with
             | Some inst ->
                 List.iter (fun o -> add_col (inst.alias ^ "." ^ o))
                           inst.gdef.outputs
             | None ->
                 (match Hashtbl.find_opt ctx.gates id with
                  | Some gdef ->
                      List.iter (fun o -> add_col (id ^ "." ^ o))
                                gdef.outputs
                  | None -> ()));
            let prefix = id ^ "." in
            Hashtbl.iter
              (fun k _ ->
                 if String.length k >= String.length prefix &&
                    String.sub k 0 (String.length prefix) = prefix then
                   add_col k)
              ctx.signals
          end else
            add_col id
    )
    targets;
  !cols

let run_program prog =
  let ctx = {
    inputs    = Hashtbl.create 10;
    signals   = Hashtbl.create 100;
    gates     = Hashtbl.create 50;
    instances = Hashtbl.create 50;
  } in

  (* 1. Collecter toutes les déclarations “InputDecl”, “GateDecl”, “InstDecl” *)
  List.iter (function
    | InputDecl (id, opt_val) ->
        let v = match opt_val with Some b -> b | None -> false in
        Hashtbl.add ctx.inputs id v;
        Hashtbl.add ctx.signals id v
    | GateDecl (name, ins, outs, body) ->
        Hashtbl.add ctx.gates name { name; inputs = ins; outputs = outs; body }
    | InstDecl (alias, gname, actuals) ->
        let gdef =
          try Hashtbl.find ctx.gates gname
          with Not_found -> error ("Gate "^gname^" inconnue")
        in
        Hashtbl.add ctx.instances alias { alias; gdef; actuals };
        exec_instance ctx (Hashtbl.find ctx.instances alias)
    | _ -> ()
  ) prog;
  (* 2. Exécuter toutes les instances déjà enregistrées *)
  Hashtbl.iter (fun _ inst -> exec_instance ctx inst) ctx.instances;

  (* 3. Afficher automatiquement les inputs *)
  Hashtbl.iter
    (fun id v -> Printf.printf "input %s = %b\n" id v)
    ctx.inputs;

  (* 4. Parcourir “PrintStmt” et “WriteStmt” pour exécuter et afficher/écrire *)
  List.iter (function
    | PrintStmt (msg, id, None) when Hashtbl.mem ctx.inputs id ->
        let v = Hashtbl.find ctx.inputs id in
        Printf.printf "%s %s = %b\n" msg id v
    | PrintStmt (msg, id, Some sub) ->
        if Hashtbl.mem ctx.gates id then exec_gate_by_name ctx id;
        let full = id ^ "." ^ sub in
        let v = find ctx.signals full "print" in
        Printf.printf "%s %s = %b\n" msg full v
    | WriteStmt (path, targets) ->
        (* a) Exécuter chaque cible pour que ctx.signals soit à jour *)
        List.iter (function
          | TGate id when Hashtbl.mem ctx.gates id ->
              exec_gate_by_name ctx id
          | TGate id when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | TSignal (id, _) when Hashtbl.mem ctx.gates id ->
              exec_gate_by_name ctx id
          | TSignal (id, _) when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | _ -> ())
          targets;
        (* b) Construire l’en-tête (header) et retirer les “alias.” fantômes *)
        let h0 = read_header_if_exists path in
        let header = ensure_columns ctx targets h0 in
        let header = List.filter (fun c -> c <> "" && not (String.ends_with c ".")) header in
        (* c) Construire la ligne de valeurs *)
        let row =
          header
          |> List.map (fun k ->
              let v = find ctx.signals k "write" in
              if v then "TRUE" else "FALSE")
          |> String.concat ","
        in
        (* d) Écrire le fichier CSV *)
        write_csv path header row;
        Printf.printf "→ CSV mis à jour : %s\n" path
    | _ -> ()
  ) prog;

  ctx
