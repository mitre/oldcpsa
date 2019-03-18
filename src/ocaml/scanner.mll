{
open Reader
open Lexing

let parse_error pos =
  parse_err pos "Bad token"
}
let special = ['*' '/' '<' '=' '>' '!' '?' ':' '$' '%' '_' '&' '~' '^']
let start = ['a' - 'z' 'A' - 'Z'] | special
let part = start | ['-' '+' '0' - '9']
rule token = parse
    [' ' '\t']				{ token lexbuf }
  | '\n'				{ new_line lexbuf; token lexbuf }
  | ';' [^ '\n']*			{ token lexbuf }
  | start part* as s			{ SYM(s) }
  | ['-' '+']? ['0'-'9']+ as n		{ NUM(int_of_string n) }
  | '"' ([^ '\\' '"']* as s) '"'	{ STR(s) }
  | '('					{ LPAR }
  | ')'					{ RPAR }
  | eof					{ EOF }
  | _					{ parse_error (lexeme_start_p lexbuf) }
