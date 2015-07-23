{
 open Parser
}
  rule lexeme = parse
|[ ' ' '\t' '\n' '\r']  { lexeme lexbuf }
|['0' - '9']+ as num { NUM (int_of_string num) }
|'+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '(' { LPAREN }
| ')' { RPAREN }
| eof { EOF }
