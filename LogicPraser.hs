import Text.Parsec.Prim
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.String ( Parser )
import Control.Applicative hiding ( ( <|> ) , many )



data LExpr = Lit Bool
           | Not LExpr       
           | And LExpr LExpr
           | Or LExpr LExpr
           | Imp LExpr LExpr  -- ( P =>  Q )
           | Red LExpr LExpr  -- ( P <=  Q )
           | Eqi LExpr LExpr  -- ( P <=> Q ) 
           deriving Show

exprCal :: Parser LExpr
exprCal = buildExpressionParser table atom 

table = [  [  Prefix ( Not <$ string  "~"  ) ]
         , [  Infix  ( And <$ string  "&"  ) AssocLeft ]
         , [  Infix  ( Or  <$ string "|"   ) AssocLeft ]
         , [  Infix  ( Eqi <$ try ( string "<=>" ) ) AssocLeft 
            , Infix  ( Imp <$  string "=>"         ) AssocLeft 
            , Infix  ( Red <$  string "<="         ) AssocLeft 
           ]
        ]

atom :: Parser LExpr
atom =  char '(' *>  exprCal   <* char ')' 
     <|> ( Lit <$> ( ( True  <$ string "True"  )  <|> ( False <$ string "False" ) ) )



expEval :: LExpr -> Bool
expEval ( Lit True ) = True
expEval ( Lit False ) = False
expEval ( Not expr ) = not . expEval $ expr
expEval ( And exprF exprS ) = ( && ) (  expEval exprF ) ( expEval  exprS )
expEval ( Or exprF exprS ) = ( || ) ( expEval exprF ) ( expEval exprS )
expEval ( Imp exprF exprS ) = ( || ) ( not . expEval $ exprF ) ( expEval exprS )
expEval ( Red exprF exprS ) = expEval ( Imp exprS exprF ) 
expEval ( Eqi exprF exprS ) 
       | first == second = True
       | otherwise = False where
          first  = expEval exprF
          second = expEval exprS 


calculator :: String -> LExpr
calculator expr = case parse  exprCal ""  expr of
                       Left msg -> error "failed to parse"
                       Right ( val ) -> val
