(* Main entry point *)

open Sexpr

let read_sexprs ch =
  let rec loop xs =
    try
      let x = read_sexpr ch in
      loop (x :: xs)
    with
    | End_of_file ->
	List.rev xs in
  loop []

let go run f i ofile =
  let lb = read_lexbuf f i in
  let xs = read_sexprs lb in
  close_in i;
  let o =
    if ofile = "" then
      stdout
    else
      open_out ofile in
  run o xs

(* Command-line processing using Arg *)

let help    = ref false
let output  = ref ""
let input   = ref ""

let usage =
  "Usage: " ^ Sys.argv.(0) ^ " [OPTIONS] [INPUT]\n" ^
  "Options:"

let speclist =
[("-o", Arg.Set_string(output), "output to file (default is standard output)");
 ("-h", Arg.Set(help), "print this message")]

let anon s =
  if !input = "" then
    input := s
  else
    begin
      prerr_endline "Bad command-line argument count\n";
      exit 1
    end

let start run =
  Arg.parse speclist anon usage;
  if !help then
    begin
      Arg.usage speclist usage;
      exit 0
    end
  else
    let (in_name, in_ch) =
    match !input with
    | "" -> ("", stdin)
    | f ->
       try
         (f, open_in f)
       with Sys_error s -> failwith s
    in
    go run in_name in_ch !output

let main run =
  try
    start run
  with
  | Failure s ->
      prerr_endline s;
      exit 1
