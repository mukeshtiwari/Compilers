(*
http://www.cs.jhu.edu/~scott/pl/book/dist/book/book.pdf
 *)

type boolexp = 
  | True
  | False
  | Not of boolexp
  | And of boolexp * boolexp
  | Or of boolexp * boolexp
  | Implies of boolexp * boolexp

let rect eval exp = 
  match exp with
  | True -> True
  | False -> False
  | Not exp -> match eval exp with
               | True -> False
               | False -> True
  | And ( expone, exptwo )-> ( 
    match ( eval expone , eval exptwo ) with
    | ( True, True ) -> False
    | ( _, _ ) -> False )
  | Or ( expone, exptwo ) -> (
    match ( eval expone, exptwo ) with
    | ( False, False ) -> False
    | ( _ , _ ) -> True )
  | Implies ( expone, exptwo ) -> (
    match ( eval expone, exptwo ) with
    | ( False, _ ) -> True
    | ( True, True ) -> True
    | ( True, False ) -> False )

			 
