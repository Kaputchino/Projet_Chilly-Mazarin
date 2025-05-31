# 1 "pcflex.mll"
 
 open Pcfparse ;;
  exception Eoi ;;

(* Emprunt� de l'analyseur lexical du compilateur OCaml *)
(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256;;
  let string_buff = ref initial_string_buffer;;
  let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;;

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
    Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
    string_buff := new_buff
  end;
  Bytes.unsafe_set (!string_buff) (!string_index) c;
  incr string_index;;

let get_stored_string () =
  let s = Bytes.to_string (Bytes.sub (!string_buff) 0 (!string_index)) in
  string_buff := initial_string_buffer;
  s;;

(* To translate escape sequences *)

let char_for_backslash c = match c with
| 'n' -> '\010'
| 'r' -> '\013'
| 'b' -> '\008'
| 't' -> '\009'
| c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
      10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                  (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255)
  then raise (Failure ("Illegal_escape: " ^ (Lexing.lexeme lexbuf)))
  else Char.chr c;;

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
  else if d1 >= 65 then d1 - 55
  else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
  else if d2 >= 65 then d2 - 55
  else d2 - 48
  in
  Char.chr (val1 * 16 + val2);;

exception LexError of (Lexing.position * Lexing.position) ;;
let line_number = ref 0 ;;

let incr_line_number lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1 ;
    Lexing.pos_bol = pos.Lexing.pos_cnum }


# 72 "pcflex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\231\255\232\255\006\000\235\255\236\255\000\000\241\255\
    \242\255\243\255\244\255\245\255\246\255\247\255\248\255\249\255\
    \250\255\251\255\252\255\078\000\001\000\254\255\255\255\002\000\
    \237\255\238\255\239\255\240\255\233\255\234\255\002\000\253\255\
    \254\255\255\255\007\000\253\255\254\255\003\000\255\255\001\000\
    \254\255\255\255\004\000\254\255\005\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\024\000\255\255\255\255\024\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\002\000\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
    \000\000\000\000\035\000\000\000\000\000\255\255\000\000\040\000\
    \000\000\000\000\043\000\000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\021\000\021\000\033\000\020\000\045\000\045\000\
    \000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \022\000\005\000\004\000\041\000\000\000\000\000\008\000\000\000\
    \015\000\014\000\000\000\010\000\016\000\007\000\018\000\003\000\
    \028\000\037\000\038\000\000\000\000\000\029\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\000\000\017\000\000\000\000\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\000\000\000\000\000\000\019\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\013\000\009\000\012\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \023\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\026\000\027\000\024\000\025\000\019\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\032\000\000\000\255\255\000\000\000\000\036\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\020\000\030\000\000\000\042\000\044\000\
    \255\255\042\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\039\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \003\000\034\000\037\000\255\255\255\255\003\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \006\000\255\255\255\255\255\255\255\255\255\255\255\255\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\023\000\023\000\023\000\023\000\019\000\255\255\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\039\000\030\000\255\255\042\000\255\255\255\255\034\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec lex lexbuf =
   __ocaml_lex_lex_rec lexbuf 0
and __ocaml_lex_lex_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 75 "pcflex.mll"
      ( lex lexbuf )
# 202 "pcflex.ml"

  | 1 ->
# 77 "pcflex.mll"
      ( (* On passe les retours � la ligne mais on garde trace de la ligne
           courante. *)
        incr_line_number lexbuf ;
        lex lexbuf )
# 210 "pcflex.ml"

  | 2 ->
let
# 81 "pcflex.mll"
                                                         lxm
# 216 "pcflex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 82 "pcflex.mll"
      ( match lxm with
          "input" -> INPUT
        | "gate" -> GATE
        | "print" -> PRINT
        | "true" -> TRUE
        | "false" -> FALSE
        | "write" -> WRITE
        | _ -> IDENT(lxm) )
# 227 "pcflex.ml"

  | 3 ->
# 90 "pcflex.mll"
                 ( DOT )
# 232 "pcflex.ml"

  | 4 ->
# 91 "pcflex.mll"
                 ( EQUAL )
# 237 "pcflex.ml"

  | 5 ->
# 92 "pcflex.mll"
                 ( COMMA )
# 242 "pcflex.ml"

  | 6 ->
# 93 "pcflex.mll"
                 ( LPAREN )
# 247 "pcflex.ml"

  | 7 ->
# 94 "pcflex.mll"
                 ( RPAREN )
# 252 "pcflex.ml"

  | 8 ->
# 95 "pcflex.mll"
                 ( LBRACE )
# 257 "pcflex.ml"

  | 9 ->
# 96 "pcflex.mll"
                 ( RBRACE )
# 262 "pcflex.ml"

  | 10 ->
# 97 "pcflex.mll"
                 ( SEMICOLON )
# 267 "pcflex.ml"

  | 11 ->
# 98 "pcflex.mll"
                 ( OR )
# 272 "pcflex.ml"

  | 12 ->
# 99 "pcflex.mll"
                 ( OR )
# 277 "pcflex.ml"

  | 13 ->
# 100 "pcflex.mll"
                 ( AND )
# 282 "pcflex.ml"

  | 14 ->
# 101 "pcflex.mll"
                 ( AND )
# 287 "pcflex.ml"

  | 15 ->
# 102 "pcflex.mll"
                   ( OR )
# 292 "pcflex.ml"

  | 16 ->
# 103 "pcflex.mll"
                   ( AND )
# 297 "pcflex.ml"

  | 17 ->
# 104 "pcflex.mll"
                   ( OR )
# 302 "pcflex.ml"

  | 18 ->
# 105 "pcflex.mll"
                   ( AND )
# 307 "pcflex.ml"

  | 19 ->
# 106 "pcflex.mll"
                 ( NOT )
# 312 "pcflex.ml"

  | 20 ->
# 107 "pcflex.mll"
          ( reset_string_buffer();
            in_string lexbuf;
            STRING (get_stored_string()) )
# 319 "pcflex.ml"

  | 21 ->
# 110 "pcflex.mll"
          ( in_cpp_comment lexbuf )
# 324 "pcflex.ml"

  | 22 ->
# 111 "pcflex.mll"
          ( in_c_comment lexbuf )
# 329 "pcflex.ml"

  | 23 ->
# 112 "pcflex.mll"
          (  EOF  )
# 334 "pcflex.ml"

  | 24 ->
# 113 "pcflex.mll"
          ( raise (LexError (lexbuf.Lexing.lex_start_p,
                             lexbuf.Lexing.lex_curr_p)) )
# 340 "pcflex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lex_rec lexbuf __ocaml_lex_state

and in_cpp_comment lexbuf =
   __ocaml_lex_in_cpp_comment_rec lexbuf 30
and __ocaml_lex_in_cpp_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 117 "pcflex.mll"
         ( lex lexbuf )
# 352 "pcflex.ml"

  | 1 ->
# 118 "pcflex.mll"
         ( EOF )
# 357 "pcflex.ml"

  | 2 ->
# 119 "pcflex.mll"
         ( in_cpp_comment lexbuf )
# 362 "pcflex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_cpp_comment_rec lexbuf __ocaml_lex_state

and in_c_comment lexbuf =
   __ocaml_lex_in_c_comment_rec lexbuf 34
and __ocaml_lex_in_c_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 122 "pcflex.mll"
         ( lex lexbuf )
# 374 "pcflex.ml"

  | 1 ->
# 123 "pcflex.mll"
         ( EOF )
# 379 "pcflex.ml"

  | 2 ->
# 124 "pcflex.mll"
         ( in_c_comment lexbuf )
# 384 "pcflex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_c_comment_rec lexbuf __ocaml_lex_state

and in_string lexbuf =
   __ocaml_lex_in_string_rec lexbuf 39
and __ocaml_lex_in_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 127 "pcflex.mll"
         ( STRING (get_stored_string ()) )
# 396 "pcflex.ml"

  | 1 ->
let
# 128 "pcflex.mll"
         c
# 402 "pcflex.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 129 "pcflex.mll"
      ( store_string_char c; in_string lexbuf )
# 406 "pcflex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_string_rec lexbuf __ocaml_lex_state

and skip_to_eol lexbuf =
   __ocaml_lex_skip_to_eol_rec lexbuf 42
and __ocaml_lex_skip_to_eol_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 132 "pcflex.mll"
            ( () )
# 418 "pcflex.ml"

  | 1 ->
# 133 "pcflex.mll"
            ( skip_to_eol lexbuf )
# 423 "pcflex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_to_eol_rec lexbuf __ocaml_lex_state

;;

