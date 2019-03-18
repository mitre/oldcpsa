open Sexpr_type
open Lexing

type token =
  | SYM of string
  | STR of string
  | NUM of int
  | LPAR
  | RPAR
  | EOF

let error_msg pos msg =
  Printf.sprintf "%s:%d:%d: %s\n" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1) msg

let failwith_msg pos msg =
  failwith (error_msg pos msg)

let parse_err = failwith_msg

let rec parse_list lexfun lexbuf lpos list =
  let tok = lexfun lexbuf in
  let pos = lexeme_start_p lexbuf in
  match tok with
  | SYM s -> parse_list lexfun lexbuf lpos (S (pos, s) :: list)
  | STR s -> parse_list lexfun lexbuf lpos (Q (pos, s) :: list)
  | NUM n -> parse_list lexfun lexbuf lpos (N (pos, n) :: list)
  | LPAR ->
      let x = parse_list lexfun lexbuf pos [] in
      parse_list lexfun lexbuf lpos (x :: list)
  | RPAR -> L (lpos, List.rev list)
  | EOF -> parse_err pos "End of file in list"

let top lexfun lexbuf =
  let tok = lexfun lexbuf in
  let pos = lexeme_start_p lexbuf in
  match tok with
  | SYM s -> S (pos, s)
  | STR s -> Q (pos, s)
  | NUM n -> N (pos, n)
  | LPAR -> parse_list lexfun lexbuf pos []
  | RPAR -> parse_err pos "Unmatched right parenthesis"
  | EOF -> raise End_of_file

let one lexfun lexbuf =
  let tok = lexfun lexbuf in
  let pos = lexeme_start_p lexbuf in
  let x =
    match tok with
    | SYM s -> S (pos, s)
    | STR s -> Q (pos, s)
    | NUM n -> N (pos, n)
    | LPAR -> parse_list lexfun lexbuf pos []
    | RPAR -> parse_err pos "Unmatched right parenthesis"
    | EOF -> parse_err pos "Nothing in string" in
  match lexfun lexbuf with
  | EOF -> x
  | _ -> parse_err (lexeme_start_p lexbuf) "Trailing garbage in string"
