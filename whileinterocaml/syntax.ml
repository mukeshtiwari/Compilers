type _ term =
  | Var : string -> string term
  | Int : int -> int term
  | Add : int term * int term -> int term
  | Sub : int term * int term -> int term
  | Mul : int term * int term -> int term
  | Div : int term * int term -> int term
  | Neg : int term  -> int term
  | And : bool term * bool term -> bool term
  | Or : bool term * bool term -> bool term
  | Greater : int term * int term -> bool term
  | Less : int term * int term -> bool term
  | Bool : bool -> bool term
  | Not : bool term -> bool term
  | Eq : 'a term * 'a term -> bool term
 and type' =
   | Anyterm of 'a term
 and stmt =
  | List of stmt list
  | Assign of string term * int term
  | If of bool term * stmt * stmt
  | While of bool term * stmt
  [@@deriving show]


(*
type term =
  | Var : string -> term
  | Int : int -> term
  | Add : term * term -> term
  | Sub : term * term -> term
  | Mul : term * term -> term
  | Div : term * term -> term
  | Neg : term  -> term
  | And : term * term -> term
  | Or : term * term -> term
  | Greater : term * term -> term
  | Less : term * term -> term
  | Bool : bool -> term
  | Not : term -> term
  | Eq : term * term -> term

 and stmt =
  | List of stmt list
  | Assign of term * term
  | If of term * stmt * stmt
  | While of term * stmt
  [@@deriving show]
 *)
  (*
type opa =
  | Add
  | Sub
  | Mul
  | Div

type opb =
  | And
  | Or

type opr =
  | Greater
  | Less

type aexpr =
  | Var of string
  | Num of int
  | Neg of aexpr
  | ABin of opa * aexpr * aexpr

type bexpr =
  | Con of bool
  | Not of bexpr
  | BBin of opb * bexpr * bexpr
  | AL of opr * aexpr * aexpr

type stmt =
  | List of stmt list
  | Assing of aexpr *  aexpr
  | If of bexpr * stmt * stmt
  | While of bexpr * stmt

  | Skip
 *)
