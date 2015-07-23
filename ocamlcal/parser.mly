%{
    open Syntax
 %}
%token <int> NUM
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token UMINUS
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start toplevel
%type <Syntax.exp> toplevel

%%

toplevel:  e = expression EOF { e } ;

expression:
  | n = NUM     { Num n}
  | LPAREN; e = expression; RPAREN
    { e }
  | e_one = expression; PLUS; e_two = expression
    { Plus (e_one, e_two) }
  | e_one = expression; MINUS; e_two = expression
    { Minus (e_one, e_two) }
  | e_one = expression; TIMES; e_two = expression
    { Times (e_one, e_two) }
  | e_one = expression; DIVIDE; e_two = expression
    { Div (e_one, e_two) }
  | UMINUS; e = expression;
    {Neg e}
  ;
