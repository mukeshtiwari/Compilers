module Parser where 

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr  as E
import Text.Parsec.String ( Parser )
import Control.Applicative hiding ( ( <|> ) , many )

import Lexer
import Syntax

table = [  [   E.Infix ( BinOp Times  <$ ( reservedOp "*" )  )   E.AssocLeft 
             , E.Infix ( BinOp Divide <$ ( reservedOp "/" )  )   E.AssocLeft ]
         , [   E.Infix ( BinOp Plus   <$ ( reservedOp "+" )  )   E.AssocLeft 
             , E.Infix ( BinOp Minus  <$ ( reservedOp "-" )  )   E.AssocLeft ]
        ]


int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = Function <$> ( reserved "def" *> identifier ) <*> 
                    ( parens . many $ variable ) <*> expr

extern :: Parser Expr
extern = Extern <$> ( reserved "extern" *> identifier ) <*> 
                    ( parens . many $ variable ) 


call :: Parser Expr 
call = Call <$> identifier <*> ( parens . commaSep $ expr ) 


expr = E.buildExpressionParser table factor 

factor :: Parser Expr
factor =  try floating
      <|> try int 
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

defn :: Parser Expr
defn =  try extern 
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p =  ( Tok.whiteSpace lexer ) *> p <* eof 

toplevel :: Parser [ Expr ]
toplevel = many $ do 
       def <- defn 
       reservedOp ";"
       return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse ( contents expr ) "<stdin>" s

parseToplevel :: String -> Either ParseError [ Expr ]
parseToplevel s = parse ( contents toplevel ) "<stdin>" s