{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.Language
import Control.Applicative hiding ( ( <|> ) , many )
import Data.Maybe ( fromJust )



data LExpr = Lit Char
           | Not LExpr
           | And LExpr LExpr
           | Or LExpr LExpr
           | Imp LExpr LExpr  -- (=>)
           | Red LExpr LExpr  -- ( <= )
           | Eqi LExpr LExpr  -- ( <=> ) 
           deriving Show

exprCal = buildExpressionParser table atom 

table = [  [ Prefix ( Not <$ string  "~"  ) ]
         , [ Infix  ( And <$ string  "&"  ) AssocLeft ]
         , [ Infix  ( Or  <$ string "|"   ) AssocLeft ]
         , [  Infix  ( Eqi <$ try ( string "<=>" ) ) AssocLeft 
            , Infix  ( Imp <$  string "=>"         ) AssocLeft 
            , Infix  ( Red <$  string "<="         ) AssocLeft 
           ]
        ]


atom =  char '(' *>  exprCal   <* char ')' 
     <|> ( Lit <$> letter )



expEval :: LExpr -> Bool
expEval ( Lit 't' ) = True
expEval ( Lit 'f' ) = False
expEval ( Not ( Lit 't' ) ) = False
expEval ( Not ( Lit 'f' ) ) = True
expEval ( Not a ) = not  ( expEval  a )
expEval ( And ( Lit 't' ) ( Lit 't' ) ) = True
expEval ( And ( Lit _ )  ( Lit _ ) ) = False
expEval ( And a b ) = ( && ) ( expEval a ) ( expEval b )
expEval ( Or ( Lit 'f' ) ( Lit 'f' ) ) = False
expEval ( Or ( Lit _ ) ( Lit _ ) ) = True
expEval ( Or a b ) = ( || ) ( expEval a ) ( expEval b )
expEval ( Imp ( Lit 'f' ) ( Lit _ ) ) = True
expEval ( Imp ( Lit 't' ) ( Lit 't' ) ) = True
expEval ( Imp ( Lit 't' ) ( Lit 'f' ) ) = False
expEval ( Imp a b ) = expEval ( Imp ( expEval a ) ( expEval b ) )
expEval ( Red p q ) = expEval ( Imp q p ) -- just reverse the arguments. 
expEval ( Eqi ( Lit 't' ) ( Lit 't' ) ) = True
expEval ( Eqi ( Lit 'f' ) ( Lit 'f' ) ) = True
expEval ( Eqi ( Lit _ ) ( Lit _ ) ) = False
expEval ( Eqi a b ) = expEval ( Eqi ( expEval a ) ( expEval b ) )


calculator :: String -> LExpr
calculator expr = case parse  exprCal ""  expr of
                       Left msg -> error "failed to parse"
                       Right ( val ) -> val
