{
  open Parser
}

  rule lexeme = parse
|'+'  { PLUS }
| '-' { MINUS }
| '>' { RARROW }
| '<' { LARROW }
| '.' { DOT }
| ',' { COMMA }
| '[' { LBRACK }
| ']' { RBRACK }
| eof { EOF }
| _  { lexeme lexbuf }
