(* The type of tokens. *)
type token = 
  | WHILE
  | THEN
  | SUB
  | SEMI
  | RPAREN
  | RBRACK
  | OR
  | NOT
  | MUL
  | LT
  | LPAREN
  | LBRACK
  | INT of (int)
  | IF
  | ID of (string)
  | GT
  | EQ
  | EOF
  | ELSE
  | DO
  | DIV
  | BOOL of (bool)
  | ASSIGN
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.stmt)

