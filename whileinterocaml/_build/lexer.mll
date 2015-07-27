{
  open Parser
  let keyword_table = Hashtbl.create 5
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ "if", IF; "else", ELSE; "while", WHILE; "then", THEN;
                "do", DO ]
  let operator_table = Hashtbl.create 20
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ '+', ADD; "else", ELSE; "while", WHILE; "then", THEN;
                "do", DO ]

}

rule lexeme = parse
(*
| "do"   { DO }
| "while" { WHILE }
|  "if"  { IF }
| "then" { THEN }
| "else" { ELSE } *)
| "true"
| "false" as boolean  { BOOL (bool_of_string boolean) }
|[ ' ' '\t' '\n' '\r']+ { lexeme lexbuf }
|['0' - '9']+ as num { INT (int_of_string num) }
|['a' - 'z']+ as id  { try
                         Hashtbl.find keyword_table id
                       with Not_found ->
                         ID id }
| '+'  { ADD }
| '-'  { SUB }
| '*'  { MUL }
| '/'  { DIV }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACK }
| '}'  { RBRACK }
| "&&" { AND }
| "||" { OR }
| '!'  { NOT }
| '<'  { LT }
| '>'  { GT }
| ';'  { SEMI }
| "==" { EQ }
| ":=" { ASSIGN }
| eof { EOF }

(*
{
let main () =
    let cin = if Array.length Sys.argv > 1 then
                open_in Sys.argv.(1)
              else stdin
    in let lexbuf = Lexing.from_channel cin in
       lexeme lexbuf

  let _ = Printexc.print main ()

}
 *)
