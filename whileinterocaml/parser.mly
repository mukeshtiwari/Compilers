%{
    open Syntax
%}

%token <int> INT
%token ADD
%token SUB
%token MUL
%token DIV
%token AND
%token OR
%token GT
%token LT
%token EQ
%token <bool> BOOL
%token NOT
%token ASSIGN
%token WHILE
%token SEMI
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token DO
%token <string> ID
%token EOF

%left OR
%left AND
%left EQ GT LT
%left ADD SUB
%left MUL DIV
%nonassoc NOT NEG

%start <Syntax.stmt> prog
%%

prog: ds = stmt EOF {ds};

stmt:
(* | LBRACK; e = stmt; RBRACK  { e } *)
| ds = separated_list(SEMI, stmtone)  { List ds } ;

stmtone:
| s = ID; ASSIGN; e = expr;
  { Assign (Var s, e) }
| IF; LPAREN; t = expr; RPAREN; THEN;
  LBRACK; eone = stmt; RBRACK; ELSE; LBRACK; etwo = stmt; RBRACK
  { If (t, eone, etwo) }
| WHILE; LPAREN; t = expr; RPAREN; DO; LBRACK; e = stmt; RBRACK
  { While (t, e) } ;

expr:
| n = INT   { Int n }
| b = BOOL  { Bool b }
| s = ID    { Var s }
| LPAREN; e = expr; RPAREN  { e }
| eone = expr; ADD; etwo = expr { Add (eone, etwo) }
| eone = expr; SUB; etwo = expr { Sub (eone, etwo) }
| eone = expr; MUL; etwo = expr { Mul (eone, etwo) }
| eone = expr; DIV; etwo = expr { Div (eone, etwo) }
| SUB; e = expr %prec NEG       { Neg e            }
| eone = expr; AND; etwo = expr { And (eone, etwo) }
| eone = expr; OR; etwo = expr  { Or (eone, etwo)  }
| eone = expr; GT; etwo = expr  { Greater (eone, etwo) }
| eone = expr; LT; etwo = expr  { Less (eone, etwo) }
| eone = expr; EQ; etwo = expr  { Eq (eone, etwo)   }
| NOT; e = expr                 { Not e } ;
