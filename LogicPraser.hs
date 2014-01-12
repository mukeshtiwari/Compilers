{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.Language
import Control.Applicative hiding ( ( <|> ) , many )
import Data.Maybe ( fromJust )


--infixl 9 <=>

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
            , Infix  ( Imp <$  string "=>"  ) AssocLeft 
            , Infix  ( Red <$  string "<="  ) AssocLeft 
           ]
        ]


atom =  char '(' *>  exprCal   <* char ')' 
     <|> ( Lit <$> letter )

calculator :: String -> LExpr
calculator expr = case parse  exprCal ""  expr of
                       Left msg -> error "failed to parse"
                       Right ( val ) -> val
