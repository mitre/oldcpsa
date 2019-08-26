open Sexpr
open Main

let need_newline = ref false

let filter o x =
  if !need_newline
  then output_char o '\n';
  print_sexpr o x;
  need_newline := true

let () =
  main (fun o xs -> List.iter (filter o) xs)
