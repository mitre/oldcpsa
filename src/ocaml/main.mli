(** A main routine for processing S-expressions *)

(** Build a program that processes S-expressions.  The program

{[    let () = main f]}

reads an input file containing a sequence of S-expressions, and opens
an output file.  It then invokes function [f] with the output channel
and the list of S-expressions read.

If the name of the program is [prog], the input file and the output
file is specified on the command line as

{[    $ prog -o OUTPUT INPUT]}

where standard input is used when [INPUT] is omitted, and standard
output is used when [-o OUTPUT] is omitted.

Function [f] is invoked in a context that catches {!exception:Failure}
exceptions, prints the failure message to standard error, and then
exits with error code 1.  Function {!val:Sexpr.failwith_msg} raises
{!exception:Failure} exceptions after adding position information to
an error message. *)
val main : (out_channel -> Lexing.position Sexpr.sexpr list -> unit) -> unit
