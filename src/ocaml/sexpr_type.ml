type 'a sexpr =
  | S of 'a * string
  | Q of 'a * string
  | N of 'a * int
  | L of 'a * 'a sexpr list
