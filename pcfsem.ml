open Pcfast


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

(* ────────── exploration des scénarios UNDET ────────── *)

let clone_ctx ctx =
  { ctx with
      inputs  = Hashtbl.copy ctx.inputs;
      signals = Hashtbl.copy ctx.signals }

let undet_inputs ctx =
  Hashtbl.fold
    (fun k v acc -> if v = TUndet then k :: acc else acc)
    ctx.inputs []

let rec explore ctx vars k =
  match vars with
  | [] -> k ctx
  | s :: tl ->
      let ctx_t = clone_ctx ctx in
      let ctx_f = clone_ctx ctx in
      Hashtbl.replace ctx_t.inputs  s TTrue;
      Hashtbl.replace ctx_t.signals s TTrue;
      Hashtbl.replace ctx_f.inputs  s TFalse;
      Hashtbl.replace ctx_f.signals s TFalse;
      explore ctx_t tl k;
      explore ctx_f tl k

let with_all_scenarios ctx action =
  let vars = undet_inputs ctx in
  if vars = [] then action ctx
  else explore ctx vars action



let error msg = raise (Failure msg)

let str_of_sig = function  
  | TTrue  -> "true"  
  | TFalse -> "false"  
  | TUndet -> "undet"  

let keys tbl =
  Hashtbl.fold (fun k _ acc -> k :: acc) tbl []

  
let find tbl name kind =
  try Hashtbl.find tbl name
  with Not_found ->
    error (Printf.sprintf "⟨%s⟩ \"%s\" introuvable" kind name)

let not_sig = function
  | TTrue  -> TFalse
  | TFalse -> TTrue
  | TUndet -> TUndet

let and_sig a b = match a, b with
  | TFalse, _ | _, TFalse -> TFalse
  | TTrue , TTrue         -> TTrue
  | _      , _            -> TUndet

let or_sig a b = match a, b with
  | TTrue , _ | _, TTrue  -> TTrue
  | TFalse, TFalse        -> TFalse
  | _     , _             -> TUndet



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
  | True  -> TTrue
  | False -> TFalse
  | Not e -> not_sig (eval_expr ctx e)
  | And(a,b) -> and_sig (eval_expr ctx a) (eval_expr ctx b)
  | Or (a,b) -> or_sig  (eval_expr ctx a) (eval_expr ctx b)
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
        let value = match opt_val with
            | Some v -> v          (* explicit TRUE / FALSE *)
            | None   -> TUndet     (* input a;  → indéterminé *) in
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
    (fun id v -> Printf.printf "input %s = %s\n" id (str_of_sig v))
    ctx.inputs;



  (* Exécution des print avec calcul si besoin *)
  List.iter (function
    | PrintStmt (msg, id, opt) ->
        let single ctx_det =
          let full =
            match opt with
            | None -> id
            | Some sub ->
                if Hashtbl.mem ctx_det.gates id then exec_gate ctx_det id;
                id ^ "." ^ sub
          in
          let value = find ctx_det.signals full "print" in
          Printf.printf "%s %s = %s\n" msg full (str_of_sig value)
        in
        with_all_scenarios ctx single
    | WriteStmt (path, targets) ->
        let rows  = ref [] in
        let heads = ref None in

        let single ctx_det =
          (* exécuter toutes les cibles pour remplir ctx_det.signals *)
          List.iter (function
            | TGate id when Hashtbl.mem ctx_det.gates id ->
                exec_gate ctx_det id
            | TGate id when Hashtbl.mem ctx_det.instances id ->
                exec_instance ctx_det (Hashtbl.find ctx_det.instances id)
            | TSignal (id,_) when Hashtbl.mem ctx_det.gates id ->
                exec_gate ctx_det id
            | TSignal (id,_) when Hashtbl.mem ctx_det.instances id ->
                exec_instance ctx_det (Hashtbl.find ctx_det.instances id)
            | _ -> ()) targets;

          (* construire l’en-tête complet *)
          let header  = ensure_columns ctx_det targets [] in
          let header  =
            List.filter (fun c -> c <> "" && not (String.ends_with c ".")) header
          in

          (match !heads with None -> heads := Some header | _ -> ());

          (* ligne de valeurs *)
          let row =
            header
            |> List.map (fun k -> str_of_sig (Hashtbl.find ctx_det.signals k))
            |> String.concat ","
          in
          rows := row :: !rows
        in
        with_all_scenarios ctx single;

        (* écriture finale du CSV *)
        (match !heads with
        | None -> ()
        | Some header ->
            let oc = open_out path in
            output_string oc (String.concat "," header ^ "\n");
            List.iter (fun r -> output_string oc (r ^ "\n")) (List.rev !rows);
            close_out oc;
            Printf.printf "→ CSV mis à jour : %s (%d scénarios)\n"
                          path (List.length !rows))


    | _ -> ()
  ) prog;

  ctx
