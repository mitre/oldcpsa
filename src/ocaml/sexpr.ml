include Sexpr_type
open Lexing
open Format

let annotation = function
  | S (a, _) -> a
  | Q (a, _) -> a
  | N (a, _) -> a
  | L (a, _) -> a

let read_lexbuf fname ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let read_sexpr lexbuf =
  Reader.top Scanner.token lexbuf

let read_sexpr_from_string ch =
  let lexbuf = Lexing.from_string ch in
  Reader.one Scanner.token lexbuf

let rec sexpr_printer f x =
  match x with
  | S (_, s) -> pp_print_string f s
  | Q (_, s) ->
      pp_print_char f '"';
      pp_print_string f s;
      pp_print_char f '"'
  | N (_, n) -> pp_print_int f n
  | L (_, xs) -> print_list f xs
and print_list f x =
  match x with
  | [] -> pp_print_string f "()"
  | x :: xs ->
      pp_open_box f 2;
      pp_print_string f "(";
      sexpr_printer f x;
      print_rest f xs
and print_rest f x =
  match x with
  | [] ->
      pp_print_string f ")";
      pp_close_box f ()
  | x :: xs ->
      pp_print_space f ();
      sexpr_printer f x;
      print_rest f xs

let print_sexpr ch x =
  let f = formatter_of_out_channel ch in
  sexpr_printer f x;
  pp_print_newline f ()

let sym x = S ((), x)
let quo x = Q ((), x)
let num x = N ((), x)
let lst x = L ((), x)

let rec strip = function
  | S (_, s) -> sym s
  | Q (_, s) -> quo s
  | N (_, i) -> num i
  | L (_, l) -> lst (List.map strip l)

let assoc key alist =
  let rec loop acc = function
    | [] -> acc
    | L (_, S (_, head) :: rest) :: l when key = head ->
	loop (acc @ rest) l
    | _ :: l -> loop acc l in
  loop [] alist

let rec has_key key = function
  | [] -> false
  | L (_, S (_, head) :: _) :: _ when key = head -> true
  | _ :: l -> has_key key l

let rem_keys keys xs =
  let f xs x =
    match x with
    | L (_, S (_, head) :: _) when List.mem head keys -> xs
    | _ -> x :: xs in
  List.rev (List.fold_left f [] xs)
