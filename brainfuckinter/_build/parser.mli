(* The type of tokens. *)
type token = 
  | RBRACK
  | RARROW
  | PLUS
  | MINUS
  | LBRACK
  | LARROW
  | EOF
  | DOT
  | COMMA

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)

