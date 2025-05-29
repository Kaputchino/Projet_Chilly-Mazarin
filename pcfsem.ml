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


let write_csv path header row =
  let oc = open_out path in          (* crée ou vide le fichier *)
  output_string oc (String.concat "," header);
  output_char   oc '\n';
  output_string oc row;
  output_char   oc '\n';
  close_out oc

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
  (* 1. lier paramètres formels -> signaux réels *)
  List.iter2
    (fun formal actual ->
       let v = find ctx.signals actual "signal" in
       Hashtbl.replace ctx.signals formal v)
    inst.gdef.inputs inst.actuals;

  (* 2. exécuter la gate *)
  exec_gate ctx inst.gdef.name;

  (* 3. recopier tout ce qui commence par "gate." sous "alias." *)
  let pref_in  = inst.gdef.name ^ "." in      (* ex. "fulladder." *)
  let len_in   = String.length pref_in in
  let pref_out = inst.alias ^ "." in          (* ex. "fa."        *)
  Hashtbl.iter
    (fun k v ->
       if String.length k >= len_in &&
          String.sub k 0 len_in = pref_in then
         let suffix = String.sub k len_in (String.length k - len_in) in
         Hashtbl.replace ctx.signals (pref_out ^ suffix) v)
    ctx.signals



let read_header_if_exists path =
  if Sys.file_exists path then
    let ic = open_in path in
    let line = input_line ic in
    close_in ic;
    String.split_on_char ',' line
  else []

let ensure_columns ctx targets header0 =
  let cols = ref header0 in
  let add_col c =
    if not (List.mem c !cols) then cols := !cols @ [c]
  in

  (* toujours commencer par les inputs globaux *)
  Hashtbl.iter (fun k _ -> add_col k) ctx.inputs;

  let add_outputs alias gdef =
    List.iter (fun o -> add_col (alias ^ "." ^ o)) gdef.outputs
  in

  List.iter
    (function
      (* ----------- cibles gate / instance ----------- *)
      | TGate id ->
          if Hashtbl.mem ctx.instances id then
            exec_instance ctx (Hashtbl.find ctx.instances id)
          else if Hashtbl.mem ctx.gates id then
            exec_gate ctx id;

          (* sorties officielles *)
          (match Hashtbl.find_opt ctx.instances id with
           | Some inst -> add_outputs inst.alias inst.gdef
           | None ->
               match Hashtbl.find_opt ctx.gates id with
               | Some gdef -> add_outputs id gdef
               | None -> ());

          (* internes déjà présents sous alias.* *)
          let prefix = id ^ "." in
          Hashtbl.iter
            (fun k _ ->
               if String.length k >= String.length prefix &&
                  String.sub k 0 (String.length prefix) = prefix
               then add_col k)
            ctx.signals

      (* ----------- cibles signal -------------------- *)
      | TSignal (id, Some sub) ->
          add_col (id ^ "." ^ sub)

      | TSignal (id, None) ->
          if Hashtbl.mem ctx.instances id || Hashtbl.mem ctx.gates id then begin
            (* 1. calculer l’instance ou la gate *)
            if Hashtbl.mem ctx.instances id then
              exec_instance ctx (Hashtbl.find ctx.instances id)
            else
              exec_gate ctx id;

            (* 2. ajouter toutes ses sorties *)
            (match Hashtbl.find_opt ctx.instances id with
             | Some inst ->
                 List.iter (fun o -> add_col (inst.alias ^ "." ^ o))
                           inst.gdef.outputs
             | None ->
                 match Hashtbl.find_opt ctx.gates id with
                 | Some gdef ->
                     List.iter (fun o -> add_col (id ^ "." ^ o))
                               gdef.outputs
                 | None -> ());

            (* 3. ajouter ses internes déjà présents *)
            let prefix = id ^ "." in
            Hashtbl.iter
              (fun k _ ->
                 if String.length k >= String.length prefix &&
                    String.sub k 0 (String.length prefix) = prefix
                 then add_col k)
              ctx.signals
          end
          else
            add_col id
    )
    targets;              
  !cols                    




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
        (* 1.  Exécuter chaque cible pour remplir ctx.signals -------------- *)
        List.iter (function
          | TGate id when Hashtbl.mem ctx.gates id ->
              exec_gate ctx id
          | TGate id when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | TSignal (id, _) when Hashtbl.mem ctx.gates id ->
              exec_gate ctx id
          | TSignal (id, _) when Hashtbl.mem ctx.instances id ->
              exec_instance ctx (Hashtbl.find ctx.instances id)
          | _ -> ())
          targets;

        (* 2.  Entête existant (si le fichier existe déjà) ----------------- *)
        let header0 = read_header_if_exists path in

        (* 3.  Entête enrichi avec les nouvelles colonnes ------------------ *)
        let header  = ensure_columns ctx targets header0 in

        (* 3 bis. Retirer alias nus ET marqueurs "alias." ----------------------- *)
        let header =
          List.filter
            (fun col ->
              col <> "" &&                        (* pas de chaîne vide      *)
              not (List.exists (function
                      | TSignal (id, None) ->
                            col = id        ||     (* alias nu   "fa"        *)
                            col = id ^ "."         (* alias point "fa."      *)
                      | _ -> false)
                    targets))
            header
        in

        (**
        let header =
          List.filter (fun col -> col <> "") header
        in*)


        (* 4.  Ligne de valeurs ------------------------------------------- *)
        let row =
          header
          |> List.map (fun k ->
              if Hashtbl.mem ctx.signals k then
                if Hashtbl.find ctx.signals k then "TRUE" else "FALSE"
              else
                error (Printf.sprintf
                          "write: signal \"%s\" absent du contexte" k))
          |> String.concat ","
        in

        (* 5.  Écriture / écrasement -------------------------------------- *)
        write_csv path header row;
        Printf.printf "→ CSV mis à jour : %s\n" path


    | _ -> ()
  ) prog;

  ctx
