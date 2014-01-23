module WhileStatic where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.String ( Parser )
import Control.Applicative ( (<|>) )


data Opa = Plus
         | Minus
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
           | Num Int
           | Neg AExpr
           | ABin Opa AExpr AExpr
           deriving Show

data BExpr = Con Bool
           | Not BExpr
           | BBin Opb BExpr BExpr
           | AL Opr AExpr AExpr

