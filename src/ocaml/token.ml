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
