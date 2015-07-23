open Syntax

let rec eval = function
  | Num v -> v
  | Plus (e_one, e_two) -> eval e_one + eval e_two
  | Minus (e_one, e_two) -> eval e_one - eval e_two
  | Times (e_one, e_two) -> eval e_one * eval e_two
  | Neg e -> - (eval e)
  | Div (e_one, e_two) ->
       match eval e_two with
       | 0 -> failwith "Division by zero"
       | n -> (eval e_one) / n
