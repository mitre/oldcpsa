(** S-expressions

This module provides a data structure for S-expressions, and a reader.
The reader records the position in the file at which items that make
up the list are located.

The S-expressions used are restricted so that most dialects of Lisp
can read them, and characters within symbols and strings never need
quoting. Every list is proper. An atom is either a symbol, an integer,
or a string. The characters that make up a symbol are the letters, the
digits, and these special characters.

{[    +-*\/\<=>!?:$%_&~^]}

A symbol may not begin with a digit or a sign followed by a digit. The
characters that make up a string are the printing characters omitting
double quote and backslash. Double quotes delimit a string. A comment
begins with a semicolon and continues to the end of the current line.

 *)

open Lexing
open Format

(** S-expression datatype *)
type 'a sexpr =
  | S of 'a * string            (** Symbol *)
  | Q of 'a * string            (** String *)
  | N of 'a * int               (** Number (integer) *)
  | L of 'a * 'a sexpr list     (** Proper list *)

(** Extract the annotation associated with an S-expression. *)
val annotation : 'a sexpr -> 'a

(** {1 Input} *)

(** Given a file name and its channel, return a {!val:Lexer.lexbuf}
    for use by the reader.  Use the empty string as the file name for
    {!val:Sys.stdin}. *)
val read_lexbuf : string -> in_channel -> lexbuf

(** Read an S-expression from a file annotated with the position of
    the datum in the file.  See module {!module:Reader} for functions
    that use positions to construct informative error messages.

    @raise End_of_file on end of file
    @raise Failure on malformed input *)
val read_sexpr : lexbuf -> position sexpr

(** Read an S-expression from a string annotated with the position of
    the datum in the string. *)
val read_sexpr_from_string : string -> position sexpr

(** {1 Output} *)

(** Pretty print an S-expression *)
val print_sexpr : out_channel -> 'a sexpr -> unit

(** An S-expression printer that can be installed as a printer in a
    toplevel system using

{[   install_printer Sexpr.sexpr_printer]}
 *)
val sexpr_printer : formatter -> 'a sexpr -> unit

(** {1 List Builders} *)

(** Construct a symbol *)
val sym : string -> unit sexpr

(** Construct a string *)
val quo : string -> unit sexpr

(** Construct an integer *)
val num : int -> unit sexpr

(** Construct a list *)
val lst : unit sexpr list -> unit sexpr

(** {1 List Operations} *)

(** Replace all annotations in a list with the unit value. *)
val strip : 'a sexpr -> unit sexpr

(** {2 Association Lists}

    An association list is a list of lists, in which the first element
    of each interior list is a symbol.  The symbol is call a key.  The
    list of values associated with a key is the result of appending
    all interior lists that are headed by the key after removing the
    key.  Thus, the list of values [(a b c d)] is associated with key
    [k] in the following association list

{[    ((k a b)
     (x y z)
     (k c d))]}

    An interior list that does not begin with a symbol is silently
    ignored. *)

(** Return the values associated with a key. *)
val assoc : string -> 'a sexpr list -> 'a sexpr list

(** Does an association list have an interior list headed with the
    given key? *)
val has_key : string -> 'a sexpr list -> bool

(** Remove interior lists if their key is in a given list. *)
val rem_keys : string list -> 'a sexpr list -> 'a sexpr list
