(** Operations on positions produced by {!val:Sexpr.read_sexpr} *)

open Lexing

(**/**)

type token = SYM of string | STR of string | NUM of int | LPAR | RPAR | EOF

(**/**)

(** Add position information to a message. *)
val error_msg : position -> string -> string

(** Add position information to a message and fail.
    @raise Failure always *)
val failwith_msg : position -> string -> 'a
