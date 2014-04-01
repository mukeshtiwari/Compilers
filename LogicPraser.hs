import Text.Parsec.Prim
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.String ( Parser )
import Control.Applicative hiding ( ( <|> ) , many )
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Monad

data LExpr = Lit Char
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
         , [  Infix  ( Or  <$ string  "|"  ) AssocLeft ]
         , [  Infix  ( Eqi <$ try ( string "<=>" ) ) AssocLeft 
            , Infix  ( Imp <$  string "=>"         ) AssocLeft 
            , Infix  ( Red <$  string "<="         ) AssocLeft 
           ]
        ]

atom :: Parser LExpr
atom =  char '(' *>  exprCal   <* char ')' 
     <|> ( Lit <$> letter )

assignment :: LExpr -> [ M.Map Char Bool ]
assignment expr = map ( M.fromList . zip vs ) ps where
    vs = variables expr
    ps = replicateM ( length vs ) [ True, False]
 

variables :: LExpr -> [ Char ]
variables expr = map head . group . sort . vars expr $ []  where
    vars ( Lit c )    xs = c : xs
    vars ( Not expr ) xs = vars expr xs
    vars ( And exprf exprs ) xs = vars exprf xs ++ vars exprs xs
    vars ( Or  exprf exprs ) xs = vars exprf xs ++ vars exprs xs
    vars ( Imp exprf exprs ) xs = vars exprf xs ++ vars exprs xs
    vars ( Red exprf exprs ) xs = vars exprf xs ++ vars exprs xs
    vars ( Eqi exprf exprs ) xs = vars exprf xs ++ vars exprs xs


expEval :: LExpr -> M.Map Char  Bool -> Bool
expEval ( Lit v   )         mp =  fromMaybe False ( M.lookup v mp )
expEval ( Not expr  )       mp =  not . expEval  expr $ mp
expEval ( And exprf exprs ) mp =  expEval exprf mp &&   expEval  exprs mp
expEval ( Or  exprf exprs ) mp =  expEval exprf mp ||   expEval  exprs mp
expEval ( Imp exprf exprs ) mp =  (  not . expEval  exprf $ mp  ) ||   expEval exprs mp 
expEval ( Red exprf exprs ) mp =  expEval ( Imp exprs exprf  ) mp 
expEval ( Eqi exprf exprs ) mp
       | first == second  = True
       | otherwise  = False where
          first  = expEval exprf mp
          second = expEval exprs mp

values :: LExpr -> [ Bool ]
values expr = map ( expEval expr ) ( assignment expr ) 

isTautology :: LExpr -> Bool
isTautology = and . values

isContradiction :: LExpr -> Bool
isContradiction  = not . or . values

isContingent :: LExpr -> Bool
isContingent expr = not (isTautology expr || isContradiction expr)
  
calculator :: String -> LExpr
calculator expr = case parse  exprCal ""  expr of
                       Left msg -> error "failed to parse"
                       Right val -> val
