type exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Neg of exp
