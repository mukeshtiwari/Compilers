import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import qualified Text.Parsec.ByteString.Lazy as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Applicative hiding ( ( <|> ) , many )

data Expr = Num Int 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Exp Expr Expr
          deriving Show

exprCal    :: T.Parser Expr
exprCal    = buildExpressionParser table factor
     

table   = [ [ op "^" Exp AssocRight ]
          , [ op "*"  Mul AssocLeft, op "/" Div AssocLeft ]
          , [op "+" Add AssocLeft, op "-" Sub AssocLeft ]
          ]          
        where
          op s f assoc  = Infix ( string s >> return f ) assoc

factor :: T.Parser Expr
factor  = char '(' *>  exprCal <* char ')' 
        <|> number
        

number  :: T.Parser Expr
number  = Num <$> ( return ( read :: String -> Int )  <*> many1 digit )