open Pcfast                      

type context = {
  input_tbl     : (string, signal) Hashtbl.t;     
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

let find tbl key kind =
  try Hashtbl.find tbl key
  with Not_found -> error (Printf.sprintf "⟨%s⟩ « %s » introuvable" kind key)

let not_sig = function
  | TTrue  -> TFalse | TFalse -> TTrue | TUndet -> TUndet

let and_sig a b = match a,b with
  | TFalse , _ | _ , TFalse -> TFalse
  | TTrue  , TTrue          -> TTrue
  | _      , _              -> TUndet

let or_sig  a b = match a,b with
  | TTrue  , _ | _ , TTrue  -> TTrue
  | TFalse , TFalse         -> TFalse
  | _      , _              -> TUndet


let write_csv path header row =
  let already = Sys.file_exists path in
  let oc =
    open_out_gen
      [Open_creat; Open_append; Open_text]        
      0o666
      path
  in
  if not already then
    output_string oc (String.concat "," header ^ "\n");
  output_string oc (row ^ "\n");
  close_out oc

let rec eval_expr ctx = function
  | True  -> TTrue
  | False -> TFalse
  | Not e -> not_sig (eval_expr ctx e)
  | And (a,b) -> and_sig (eval_expr ctx a) (eval_expr ctx b)
  | Or  (a,b) ->  or_sig (eval_expr ctx a) (eval_expr ctx b)
  | Parens e  -> eval_expr ctx e

  | Var (id,None) ->
      if Hashtbl.mem ctx.signals id then find ctx.signals id "signal"
      else if Hashtbl.mem ctx.input_tbl  id then find ctx.input_tbl  id "input"
      else if Hashtbl.mem ctx.gates   id then (exec_gate_by_name ctx id;
                                               find ctx.signals id "signal")
      else if Hashtbl.mem ctx.instances id then
        (exec_instance ctx (find ctx.instances id "inst"); 
         find ctx.signals id "signal")
      else error ("Signal non défini : "^id)

  | Var (id,Some sub) ->
      let full = id ^ "." ^ sub in
      if not (Hashtbl.mem ctx.signals full) then begin
        if Hashtbl.mem ctx.instances id then
          exec_instance ctx (find ctx.instances id "inst")
        else if Hashtbl.mem ctx.gates id then
          exec_gate_by_name ctx id
      end;
      find ctx.signals full "signal"


and exec_gate ctx gdef actuals =
  let local = {
    ctx with
      signals   = Hashtbl.copy ctx.signals;
      instances = Hashtbl.copy ctx.instances;
  } in

  List.iter2
    (fun formal expr ->
      let v = eval_expr ctx expr in
      Hashtbl.replace local.signals formal v
    )
    gdef.inputs
    actuals;        

  List.iter (eval_stmt local) gdef.body;
  List.iter
    (fun o ->
       let v = find local.signals o "sortie" in
       Hashtbl.replace ctx.signals (gdef.name ^ "." ^ o) v)
    gdef.outputs

and exec_gate_by_name ctx name =
  let gdef = find ctx.gates name "gate" in
  let actuals = List.map (fun id -> Var(id,None)) gdef.inputs in
  exec_gate ctx gdef actuals


and eval_stmt ctx = function
  | Assign (id,e) ->
      Hashtbl.replace ctx.signals id (eval_expr ctx e)

  | InstAssign (alias,gname,args_expr) ->
      let gdef = find ctx.gates gname "gate" in
      if List.length args_expr <> List.length gdef.inputs then
        error (Printf.sprintf "Arity mismatch : %s attend %d args"
                gname (List.length gdef.inputs));
      let actual_ids =
        List.map (function
          | Var(x,None)       -> x
          | Var(x,Some sub)   -> x ^ "." ^ sub
          | _ -> error "InstAssign : args doivent être Var") args_expr
      in
      Hashtbl.replace ctx.instances alias { alias; gdef; actuals = actual_ids };
      exec_gate ctx gdef args_expr;
      List.iter
        (fun o ->
           let v = find ctx.signals (gdef.name ^ "." ^ o) "sortie" in
           Hashtbl.replace ctx.signals (alias ^ "." ^ o) v)
        gdef.outputs


and exec_instance ctx inst =
  List.iter2
    (fun formal actual ->
       ensure_signal ctx actual;
       Hashtbl.replace ctx.signals formal
         (find ctx.signals actual "actual"))
    inst.gdef.inputs inst.actuals;
  let actuals = List.map (fun id -> Var(id,None)) inst.actuals in
  exec_gate ctx inst.gdef actuals;
  List.iter
    (fun o ->
       let v = find ctx.signals (inst.gdef.name ^ "." ^ o) "sortie" in
       Hashtbl.replace ctx.signals (inst.alias ^ "." ^ o) v)
    inst.gdef.outputs


and ensure_signal ctx id =
  if Hashtbl.mem ctx.signals id then ()
  else
    let base =
      match String.index_opt id '.' with
      | None   -> id
      | Some i -> String.sub id 0 i
    in
    if Hashtbl.mem ctx.instances base then
      exec_instance ctx (find ctx.instances base "inst")
    else if Hashtbl.mem ctx.gates base then
      exec_gate_by_name ctx base

let with_all_scenarios ctx undet_inputs body =
  let total   = 1 lsl List.length undet_inputs in
  let counter = ref 0 in
  let rec explore ctx = function
    | [] ->
        incr counter;
        Printf.printf "=== scénario %d/%d ===\n" !counter total;
        body ctx
    | id :: rest ->
        (* branche TRUE *)
        let ctx_true = {
          ctx with
            input_tbl = Hashtbl.copy ctx.input_tbl;
            signals   = Hashtbl.copy ctx.signals;
        } in
        Hashtbl.replace ctx_true.input_tbl id TTrue;
        Hashtbl.replace ctx_true.signals  id TTrue;
        explore ctx_true rest;
        (* branche FALSE *)
        let ctx_false = {
          ctx with
            input_tbl = Hashtbl.copy ctx.input_tbl;
            signals   = Hashtbl.copy ctx.signals;
        } in
        Hashtbl.replace ctx_false.input_tbl id TFalse;
        Hashtbl.replace ctx_false.signals  id TFalse;
        explore ctx_false rest
  in
  explore ctx undet_inputs

let read_header_if_exists path =
  if Sys.file_exists path then
    let ic = open_in path in
    let h  = input_line ic in
    close_in ic; String.split_on_char ',' h
  else []

let rec ensure_columns ctx targets header0 =
  let cols = ref header0 in
  let add c = if not (List.mem c !cols) then cols := !cols @ [c] in
  Hashtbl.iter (fun k _ -> add k) ctx.input_tbl;
  let add_out alias gdef = List.iter (fun o -> add (alias^"."^o)) gdef.outputs in
  List.iter
    (function
      | TGate id ->
          if Hashtbl.mem ctx.gates id then exec_gate_by_name ctx id;
          if Hashtbl.mem ctx.instances id then exec_instance ctx (find ctx.instances id "inst");
          (match Hashtbl.find_opt ctx.instances id with
           | Some i -> add_out i.alias i.gdef | None ->
             (match Hashtbl.find_opt ctx.gates id with
              | Some g -> add_out id g | _ -> ()));
          Hashtbl.iter (fun k _ -> if String.starts_with ~prefix:(id^".") k then add k) ctx.signals
      | TSignal (id,Some sub) -> add (id^"."^sub)
      | TSignal (id,None) ->
          if Hashtbl.mem ctx.instances id then exec_instance ctx (find ctx.instances id "inst")
          else if Hashtbl.mem ctx.gates id then exec_gate_by_name ctx id;
          add id)
    targets;
  !cols


let run_program prog =
  (* Contexte initial *)
  let ctx0 = {
    input_tbl = Hashtbl.create 16;
    signals   = Hashtbl.create 128;
    gates     = Hashtbl.create 32;
    instances = Hashtbl.create 32;
  } in

  let csv_paths =
    List.fold_left
      (fun acc stmt ->
         match stmt with
         | WriteStmt (path, _) when not (List.mem path acc) -> path :: acc
         | _ -> acc)
      []
      prog
  in
  List.iter (fun p -> if Sys.file_exists p then Sys.remove p) csv_paths;

  List.iter (function
    | InputDecl (id,opt) ->
        let v = Option.value ~default:TUndet opt in
        Hashtbl.add ctx0.input_tbl id v;
        Hashtbl.add ctx0.signals   id v
    | GateDecl (n,i,o,b) ->
        Hashtbl.add ctx0.gates n { name=n; inputs=i; outputs=o; body=b }
    | InstDecl (alias,gname,actuals) ->
        let gdef = find ctx0.gates gname "gate" in
        let inst = { alias; gdef; actuals } in
        Hashtbl.add ctx0.instances alias inst;
        exec_instance ctx0 inst          
    | _ -> ()) prog;

  (* Entrées indéterminées *)
  let undet_inputs =
    Hashtbl.fold (fun id v acc -> if v = TUndet then id :: acc else acc)
      ctx0.input_tbl []
  in

  (* Corps exécuté dans chaque scénario *)
  let scenario_body ctx =
    Hashtbl.iter (fun _ inst -> exec_instance ctx inst) ctx.instances;
    Hashtbl.iter
      (fun id v ->
         Printf.printf "input %s = %s\n" id (str_of_sig v))
      ctx.input_tbl;

    List.iter (function
      | PrintStmt (msg, id, opt) ->
          if Hashtbl.mem ctx.instances id then
            exec_instance ctx (find ctx.instances id "inst")
          else if Hashtbl.mem ctx.gates id then
            exec_gate_by_name ctx id;
          let full =
            match opt with
            | None   -> id
            | Some s -> id ^ "." ^ s
          in
          ensure_signal ctx full;                    
          let v = find ctx.signals full "print" in
          Printf.printf "%s %s = %s\n" msg full (str_of_sig v)


      | WriteStmt (path,targets) ->
          List.iter (function
            | TGate id when Hashtbl.mem ctx.gates id ->
                exec_gate_by_name ctx id
            | TGate id when Hashtbl.mem ctx.instances id ->
                exec_instance ctx (find ctx.instances id "inst")
            | TSignal (id,_) when Hashtbl.mem ctx.gates id ->
                exec_gate_by_name ctx id
            | TSignal (id,_) when Hashtbl.mem ctx.instances id ->
                exec_instance ctx (find ctx.instances id "inst")
            | _ -> ()) targets;
          let header0 = read_header_if_exists path in
          let header  = ensure_columns ctx targets header0 in
          let line =
            header
            |> List.map (fun k -> str_of_sig (find ctx.signals k "csv"))
            |> String.concat "," in
          write_csv path header line;
          Printf.printf "→ CSV mis à jour : %s\n" path
      | _ -> ()) prog
  in

  with_all_scenarios ctx0 undet_inputs scenario_body;

  ctx0

