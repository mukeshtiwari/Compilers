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

{-- I am not very good with representing grammers 
Expr -> ( Expr ) | Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr 
Expr -> Int 
The representation of 1 + 2 + 3 should be 1+(2+3)
--}


readI :: String -> Int
readI = read


parseExpression :: T.Parser Expr
parseExpression =  try parseAdd <|> try parseSub <|> try parseMul 
                <|> try parseDiv <|> try parseExp  
                <|> try parseNum <|> char '(' *> parseExpression <* char ')' 



parseNum :: T.Parser Expr
parseNum = Num <$> ( return readI <*> many1 digit ) 
           
         
parseAdd :: T.Parser Expr
parseAdd = Add <$>  (  (  parseNum <|>  ( char '(' *> parseExpression <* char ')' ) )
           <* char '+' )  <*>   
           (  parseNum <|>   ( char '(' *> parseExpression <* char ')' )  )

parseSub :: T.Parser Expr          
parseSub =  Sub <$>  (  ( parseNum <|>  ( char '(' *> parseExpression <* char ')' ) ) 
            <* char '-' )  <*>   
           (  parseNum <|>   ( char '(' *> parseExpression <* char ')' ) )

parseMul :: T.Parser Expr
parseMul =  Mul <$>  ( ( parseNum <|>  ( char '(' *> parseExpression <* char ')' ) ) 
            <* char '*' )  <*>   
           (  parseNum <|>   ( char '(' *> parseExpression <* char ')' )  )

parseDiv :: T.Parser Expr
parseDiv = Div <$>  (  (  parseNum <|>  ( char '(' *> parseExpression <* char ')' ) ) 
           <* char '/' )  <*>   
           ( parseNum <|>   ( char '(' *> parseExpression <* char ')' )  )

parseExp :: T.Parser Expr 
parseExp = Exp <$>  (  (  parseNum <|>  ( char '(' *> parseExpression <* char ')' ) ) 
           <* char '^' )  <*>   
           ( parseNum <|>   ( char '(' *> parseExpression <* char ')' )  )


calExpression  :: Expr -> Int
calExpression ( Num n ) = n 
calExpression ( Add e1 e2 ) = calExpression e1 + calExpression e2
calExpression ( Sub e1 e2 ) = calExpression e1 - calExpression e2
calExpression ( Mul e1 e2 ) = calExpression e1 * calExpression e2
calExpression ( Div e1 e2 ) = div ( calExpression e1 ) ( calExpression e2 )
calExpression ( Exp e1 e2 ) =  ( calExpression e1 ) ^ ( calExpression e2 )
          
                                       
calculator :: String -> Int
calculator expr = case parse parseExpression "" ( BS.pack expr ) of
                       Left msg -> error "failed to parse"
                       Right ( val ) -> calExpression val


