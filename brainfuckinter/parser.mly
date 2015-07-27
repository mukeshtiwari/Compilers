%{
  open Ast
%}

%token PLUS
%token MINUS
%token RARROW
%token LARROW
%token COMMA
%token DOT
%token LBRACK
%token RBRACK
%token EOF

%start <Ast.prog> prog
%%

prog: ds = list (expression); EOF { ds } ;

expression:
| LBRACK; e = list (expression); RBRACK   { Loop e }
| PLUS                                    { Incv }
| MINUS                                   { Decv }
| RARROW                                  { Incp }
| LARROW                                  { Decp }
| COMMA                                   { Input }
| DOT                                     { Output }
;
