let version = "0.01" ;;

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\tRead a PCF program from file (default is stdin)\n%!"
    Sys.argv.(0) in
  exit 1
;;

let main() =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-" -> stdin
        | name ->
            (try open_in name with
            |_ -> Printf.eprintf "Opening %s failed\n%!" name; exit 1)
       )
    | n -> usage () in
  let _ = Printf.printf "        Welcome to PCF, version %s\n%!" version in
  let lexbuf = Lexing.from_channel input_channel in
  while true do
    try
      let _ = Printf.printf  "> %!" in
      let e = Pcfparse.main Pcflex.lex lexbuf in
      let _ = Printf.printf "Recognized: " in
      let _ = Pcfast.print stdout e in
      let _ = Printf.fprintf stdout " =\n%!" in
      let _ = Pcfsem.printval (Pcfsem.eval e) in
      Printf.printf "\n%!"
    with
    | Pcflex.Eoi -> Printf.printf  "Bye bye.\n%!" ; exit 0
    | Failure msg -> Printf.printf "Erreur: %s\n\n" msg
    | Parsing.Parse_error ->
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        Format.printf
          "File %S, line %i, characters %i-%i: Syntax error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Pcflex.LexError (sp, ep) ->
        Printf.printf
          "File %S, line %i, characters %i-%i: Lexical error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
  done
;;

if !Sys.interactive then () else main () ;;
