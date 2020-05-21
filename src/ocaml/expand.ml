(* Expand macros as is done by CPSA *)

open Lexing
open Sexpr
open Main

(* The macro expand loop limit *)
let limit = 1000

(* Include depth bound *)
let bound = 16

type macro = {
    name: string;
    args: string list;
    body: position sexpr;
  }

(**** Define a macro ****)

let symbol = function
  | S (_, s) -> s
  | x -> failwith_msg (annotation x) "Expecting a symbol"

let defmacro pos = function
  | [L (_, name :: args); body] ->
     {
       name = symbol name;
       args = List.map symbol args;
       body = body;
     }
  | _ -> failwith_msg pos "Malformed macro"

(**** Expand macros ****)

(* lookup key in association list with default *)

let rec lookup default x = function
  | [] -> default
  | (s, y) :: _ when s = x -> y
  | _ :: xs -> lookup default x xs

let rec subst env = function
  | S (_, s) as x -> lookup x s env
  | L (pos, xs) -> L (pos, List.map (subst env) xs)
  | x -> x

let rec zip xs ys =
  match (xs, ys) with
  | (x :: xs, y :: ys) -> (x, y) :: zip xs ys
  | _ -> []

let apply mac args =
  subst (zip mac.args args) mac.body

let rec expand_one macs sym xs =
  match macs with
  | [] -> None
  | mac :: _ when mac.name = sym &&
                       List.length mac.args = List.length xs ->
     Some (apply mac xs)
  | _ :: macs -> expand_one macs sym xs

let rec limited_expand macs pos limit = function
  | L (_, S (_, sym) :: xs) as x when limit > 0 ->
     (match expand_one macs sym xs with
      | Some y -> limited_expand macs pos (limit - 1) y
      | None -> x)
  | _ when limit <= 0 ->
     failwith_msg pos "Expansion limit exceeded"
  | x -> x

(* Splice sexprs that start with splice symbol.  The splice symbol is
   ^. *)

let rec splice = function
  | [] -> []
  | L (_, S (_, keyword) :: xs) :: ys when keyword = "^" ->
     let rec loop xs ys =
       match xs with
       | [] -> splice ys
       | x :: xs -> x :: loop xs ys in
     loop xs ys
  | x :: xs -> x :: splice xs

let rec expand_all macs x =
  match limited_expand macs (annotation x) limit x with
  | L (pos, xs) ->
     let ys = List.map (expand_all macs) xs in
     L (pos, splice ys)
  | x -> x

let need_newline = ref false

let expand o macs x =
  if !need_newline
  then output_char o '\n';
  print_sexpr o (expand_all macs x);
  need_newline := true

(**** Include the contents of a file ****)

let read_sexprs ch =
  let rec loop xs =
    try
      let x = read_sexpr ch in
      loop (x :: xs)
    with
    | End_of_file ->
	List.rev xs in
  loop []

(**** Main loop ****)

(* o is the output channel
   bound is the include bound
   macs is the current list of defined macros *)

let rec loop o bound macs = function
  | [] -> macs                  (* Return the collected macros *)
  | x :: xs -> dispatch o bound macs xs x

 and dispatch o bound macs xs = function
   | L (pos, (S (_, keyword) :: rest)) when keyword = "defmacro" ->
      loop o bound (defmacro pos rest :: macs) xs
   | L (pos, (S (_, keyword) :: rest)) when keyword = "include" ->
      loop o bound (incl o bound macs pos rest) xs
   | x ->
      expand o macs x;
      loop o bound macs xs

 and incl o bound macs pos = function
   | [Q (_, f)] when bound > 0 ->
      let i =
        try
          open_in f
        with Sys_error s -> failwith s
      in
      let lb = read_lexbuf f i in
      let xs = read_sexprs lb in
      loop o (bound - 1) macs xs
   | [Q (_, f)] when bound <= 0 ->
      failwith_msg pos ("Include depth exceeded with file " ^ f)
   | _ -> failwith_msg pos "Malformed include directive"

let start o xs =
  let _ = loop o bound [] xs in
  ()

let () =
  main start
