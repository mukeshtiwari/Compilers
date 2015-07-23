(* The type of tokens. *)
type token = 
  | UMINUS
  | TIMES
  | RPAREN
  | PLUS
  | NUM of (int)
  | MINUS
  | LPAREN
  | EOF
  | DIVIDE

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.exp)

