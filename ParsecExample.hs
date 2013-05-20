import qualified Text.Parsec.Token as Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.Language
import qualified Text.Parsec.ByteString.Lazy as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Applicative hiding ( ( <|> ) , many )
import Data.Maybe ( fromJust )
import Control.Monad.Identity 

languageDef :: Token.GenLanguageDef BS.ByteString () Identity
languageDef = emptyDef { Token.commentStart   =  "/*"
               , Token.commentEnd     = "*/"
               , Token.commentLine    =  "//"
               , Token.nestedComments = False
               , Token.identStart     = letter <|> char '_'
               , Token.identLetter    = alphaNum <|> oneOf "_'"
               , Token.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , Token.reservedOpNames= ["*" , "/" , "^" , "+" , "-" ]
               , Token.reservedNames  = []
               , Token.caseSensitive  = True
               }

lexer :: Token.GenTokenParser BS.ByteString () Identity
lexer =  Token.makeTokenParser languageDef

identifier :: ParsecT BS.ByteString () Identity String
identifier = Token.identifier lexer
reserved ::  String -> ParsecT BS.ByteString () Identity ()
reserved = Token.reserved lexer
operator :: ParsecT BS.ByteString () Identity String
operator = Token.operator lexer
reservedOp ::String -> ParsecT BS.ByteString () Identity ()
reservedOp = Token.reservedOp lexer
natural :: ParsecT BS.ByteString () Identity Integer
natural = Token.natural lexer
integer :: ParsecT BS.ByteString () Identity Integer
integer = Token.integer lexer
float :: ParsecT BS.ByteString () Identity Double
float = Token.float lexer
lexeme :: ParsecT BS.ByteString () Identity a -> ParsecT BS.ByteString () Identity a
lexeme = Token.lexeme lexer
parens :: ParsecT BS.ByteString () Identity a -> ParsecT BS.ByteString () Identity a
parens = Token.parens lexer
whiteSpace :: ParsecT BS.ByteString  () Identity  ()
whiteSpace = Token.whiteSpace lexer
semi :: ParsecT BS.ByteString () Identity String
semi = Token.semi lexer
comma :: ParsecT BS.ByteString () Identity String
comma = Token.comma lexer
colon :: ParsecT BS.ByteString () Identity String
colon = Token.colon lexer
dot :: ParsecT BS.ByteString () Identity String
dot = Token.dot lexer
semiSep :: ParsecT BS.ByteString () Identity a -> ParsecT BS.ByteString () Identity [a]
semiSep = Token.semiSep lexer
commaSep :: ParsecT BS.ByteString () Identity a -> ParsecT BS.ByteString () Identity [a]
commaSep = Token.commaSep lexer



data Expr = Num Integer 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Exp Expr Expr
          deriving Show

exprCal    :: T.Parser Expr
exprCal    = buildExpressionParser table factor
     
table :: [[ Operator BS.ByteString () Identity Expr ]]
table   = [ [ op "^"  Exp AssocRight ]
          , [ op "*"  Mul AssocLeft, op "/"  Div AssocLeft ]
          , [ op "+"  Add AssocLeft, op "-"  Sub AssocLeft ]
          ]          
        where
          op s f assoc  = Infix ( reservedOp s  >> return f ) assoc

factor :: T.Parser Expr
factor  = parens  exprCal 
        <|> Num <$> integer
        


calExpression  :: Expr -> Integer
calExpression ( Num n ) = n 
calExpression ( Add e1 e2 ) = calExpression e1 + calExpression e2
calExpression ( Sub e1 e2 ) = calExpression e1 - calExpression e2
calExpression ( Mul e1 e2 ) = calExpression e1 * calExpression e2
calExpression ( Div e1 e2 ) = div ( calExpression e1 ) ( calExpression e2 )
calExpression ( Exp e1 e2 ) =   calExpression e1  ^ ( calExpression e2 )
          
                                       
calculator :: String -> Integer
calculator expr = case parse ( whiteSpace >> exprCal ) ""  ( BS.pack expr ) of
                       Left msg -> error "failed to parse"
                       Right ( val ) -> calExpression val

