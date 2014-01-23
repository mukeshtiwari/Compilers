module WhileStatic where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.String ( Parser )
import Control.Applicative  hiding ( (<|>) )


data Opa = Add
         | Sub
         | Mul
         | Div 
         deriving Show

data Opb = And 
         | Or 
         deriving Show

data Opr = Greater 
         | Less 
         deriving Show

data AExpr = Var String
           | Num Integer
           | Neg AExpr
           | ABin Opa AExpr AExpr
           deriving Show

data BExpr = Con Bool
           | Not BExpr
           | BBin Opb BExpr BExpr
           | AL Opr AExpr AExpr
           deriving Show

data Stmt = List [ Stmt ]
          | Assing AExpr AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving Show

--build Token parser

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef 
            {
                T.commentStart = "{-"
              , T.commentEnd = "-}"
              , T.reservedOpNames = [ "+", "-", "*", "/", ":=", ">", "<",
                                      "not", "and", "or" ]
              , T.reservedNames = [ "if", "then", "else", "while", "do", "skip",
                                  "true", "false" ]

            }


identifier :: Parser String
identifier = T.identifier lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

integer :: Parser Integer
integer = T.integer lexer

semi :: Parser String
semi =  T.semi lexer

semiSep :: Parser a -> Parser [ a ]
semiSep = T.semiSep lexer

--end of lexer

--build expresion parser 

aTable = [  [   Prefix ( Neg <$ reservedOp "-" ) ]  
          , [   Infix  ( ABin Mul <$ reservedOp "*" ) AssocLeft
              , Infix  ( ABin Div <$ reservedOp "/" ) AssocLeft ]
          , [   Infix  ( ABin Add <$ reservedOp "+" ) AssocLeft
              , Infix  ( ABin Sub <$ reservedOp "-" ) AssocLeft ]
         ]



bTable = [  [  Prefix ( Not <$ reservedOp "not" ) ]
          , [  Infix  ( BBin And <$ reservedOp "and" ) AssocLeft ] 
          , [  Infix  ( BBin Or  <$ reservedOp "or"  ) AssocLeft ]
         ] 
      
aExpression :: Parser AExpr
aExpression = buildExpressionParser aTable aTerm where 
         aTerm :: Parser AExpr
         aTerm =  parens aExpression 
              <|> Var <$> identifier
              <|> Num <$> integer


bExpression :: Parser BExpr
bExpression = buildExpressionParser bTable bTerm where 
         bTerm =  parens bExpression 
              <|> (  Con True   <$ reserved "true"  ) 
              <|> (  Con False  <$ reserved "false" )
              <|> (  AL Greater <$> ( aExpression  <* reserved ">" ) <*> aExpression )
              <|> (  AL Less    <$> ( aExpression  <* reserved "<" ) <*> aExpression ) 




              

whileParser :: Parser Stmt
whileParser = whiteSpace *> stmtParser <* eof where 
            stmtParser :: Parser Stmt
            stmtParser = List <$> semiSep stmtOne
            stmtOne :: Parser Stmt
            stmtOne =  ( Assing <$> ( Var <$> identifier ) 
                                <*> ( reserved ":=" *> aExpression ) )
                   <|> ( If <$> ( reserved "if" *> bExpression <* reserved "then" ) 
                          <*> stmtParser 
                          <*> ( reserved "else" *> stmtParser ) )
                   <|> ( While <$> ( reserved "while" *> bExpression <* reserved "do" ) 
                             <*> stmtParser )
                   <|> ( Skip <$ reserved "nop" ) 


parseString :: String -> IO ()
parseString s = case parse  whileParser "" s of 
                     Left err -> print err
                     Right tree -> print tree