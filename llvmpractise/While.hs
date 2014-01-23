module While where

import Text.Parsec
import Text.Parsec.String ( Parser )
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr 
import Text.Parsec.Language ( emptyDef )
import Control.Applicative hiding ( (<|> ))


{-- 

expr  ::= var | const | ( expr ) | unop expr | expr duop expr
var   ::= letter { letter | digit }*
const ::= true | false
unop  ::= ~
duop  ::= & | =

stmt ::= nop | var := expr | if expr then stmt else stmt fi | while expr do stmt od
       | stmt { ; stmt }+

--}

--create abstract syntax tree

data Expr = Var String
          | Con Bool
          | U Unop Expr
          | B Bop Expr Expr
          deriving Show

data Unop = Not deriving Show

data Bop = And 
         | Iff 
         deriving Show

data Stmt = Nop
          | Assign Expr Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | List [ Stmt ]
          deriving Show


-- implement lexer

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style where
         style = emptyDef 
               {
                   T.commentStart = "{-"
                 , T.commentEnd   = "-}"
                 , T.reservedOpNames = [ "~", "&", "=", ":=" ]
                 , T.reservedNames =   [ "true", "false", "nop", "if", "then", "else",
                                       "fi", "while", "do", "od" ]
               } 



identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer


semiSep :: Parser a -> Parser [ a ]
semiSep = T.semiSep lexer

semiSep1 :: Parser a -> Parser [ a ]
semiSep1 = T.semiSep1 lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer


table = [   [ Prefix ( U Not <$ reservedOp "~" ) ] 
          , [ Infix  ( B And <$ reservedOp "&" ) AssocLeft ]
          , [ Infix  ( B Iff <$ reservedOp "=" ) AssocLeft ]
        ]

exprParser :: Parser Expr
exprParser = buildExpressionParser table term 

term :: Parser Expr
term = parens exprParser 
    <|> Var <$> identifier
    <|> ( pure ( Con True  ) <* reserved "true"  )
    <|> ( pure ( Con False ) <* reserved "false" )

 

mainParser :: Parser Stmt
mainParser = whiteSpace *> stmtParser <* eof where
        stmtParser :: Parser Stmt
        stmtParser = List <$> semiSep stmtOne
        stmtOne :: Parser Stmt
        stmtOne =    ( pure Nop <* reserved "nop" )
                <|>  ( Assign <$> ( Var <$> identifier <* reserved ":=" )  <*> exprParser  )
                <|>  ( If <$> ( reserved "if" *> exprParser <* reserved "then" ) 
                          <*> stmtParser <*> 
                          ( reserved "else" *> stmtParser <* reserved "fi") )
                <|>  ( While <$> ( reserved "while" *>  exprParser  <* reserved "do" ) 
                             <*> ( stmtParser <* reserved "od" ) )


play :: String -> IO ()
play s = case parse mainParser "" s of 
              Left err -> print err
              Right ans -> print ans