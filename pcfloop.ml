let version = "0.1"

let usage () =
  Printf.eprintf "Usage: %s [file]\n%!" Sys.argv.(0);
  exit 1

let main () =
  let input_channel =
    if Array.length Sys.argv < 2 then usage ()
    else
      let filename = Sys.argv.(1) in
      try open_in filename
      with _ ->
        Printf.eprintf "Cannot open file %s\n%!" filename;
        exit 1
  in

  let lexbuf = Lexing.from_channel input_channel in
  try
    let prog = Pcfparse.main Pcflex.lex lexbuf in
    Printf.printf "Programme reconnu avec succès.\n\n";
    Pfacast.print_program stdout prog;
    print_newline ()
  with
  | Pcflex.Eoi ->
      Printf.printf "Fin de fichier inattendue.\n%!"
  | Failure msg ->
      Printf.printf "Erreur : %s\n%!" msg
  | Parsing.Parse_error ->
      let sp = Lexing.lexeme_start_p lexbuf in
      let ep = Lexing.lexeme_end_p lexbuf in
      Printf.printf
        "Erreur de syntaxe ligne %d, caractères %d-%d.\n"
        sp.Lexing.pos_lnum
        (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
        (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
  | Pcflex.LexError (sp, ep) ->
      Printf.printf
        "Erreur lexicale ligne %d, caractères %d-%d.\n"
        sp.Lexing.pos_lnum
        (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
        (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)

let () = if not !Sys.interactive then main ()
