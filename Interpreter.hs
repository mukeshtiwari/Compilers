{-# LANGUAGE GADTs #-}
import qualified Data.Map as M
import Data.Char

type Variable = String
type Val = Int
type Store = M.Map String Int

data Expr where 
   Const :: Val -> Expr
   Var :: Variable -> Expr
   Minus :: Expr -> Expr -> Expr
   Times :: Expr -> Expr -> Expr
   Greater :: Expr -> Expr -> Expr

data Command where
   Assign :: Variable -> Expr -> Command
   Seq :: Command -> Command -> Command
   Cond :: Expr -> Command -> Command -> Command
   While :: Expr -> Command -> Command


fetch :: Store -> Variable -> Val
fetch  m v = M.findWithDefault 0 v m

update :: Store -> Variable -> Val -> Store
update s v t = M.insert v t s

initial :: Store
initial = M.empty

eval :: Expr -> Store -> Val
eval ( Const v ) _ = v
eval ( Var v ) s = fetch s v 
eval ( Minus e1 e2 ) s = ( eval e1 s ) - ( eval e2 s )
eval ( Times e1 e2 ) s = ( eval e1 s ) * ( eval e2 s )
eval ( Greater e1 e2 ) s = if ( eval e1 s ) >  ( eval e2 s ) then 1 else 0

switch :: Val -> ( Store -> Store ) -> ( Store -> Store ) -> Store -> Store 
switch v fs gs s = if v == 1 then fs s else gs s 

interpret :: Command -> Store -> Store
interpret ( Assign v expr ) s = update s v  ( eval expr s )
interpret ( Seq c d ) s = interpret d ( interpret c s )
interpret ( Cond expr c d ) s = switch ( eval expr s ) ( interpret c ) ( interpret d ) s  
interpret ( While expr c ) s 
    | t == 0 = w   
    | otherwise =  interpret ( While expr c ) w 
    where
     t = eval expr s
     w = switch t ( interpret c ) id s 


{--

data Token where 
	Ident :: String -> Token
	Symbol :: String -> Token
	Number :: Int -> Token

instance Show Token where 
       show ( Ident s ) = "Ident " ++ s
       show ( Symbol s ) = "Symbol " ++ s
       show ( Number n ) = "Number " ++ show n

--}
data Token = Ident String
           | Symbol String
           | Number Int 
           deriving Show


lexer :: String -> [ Token ]
lexer [] = []
lexer ( '+' : xs ) = Symbol "+" : lexer xs
lexer ( '-' : xs ) = Symbol "-" : lexer xs
lexer ( '*' : xs ) = Symbol "*" : lexer xs
lexer ( '>' : xs ) = Symbol ">" : lexer xs
lexer ( ';' : xs ) = Symbol ";" : lexer xs
lexer ( ':' : '=' : xs ) = Symbol ":=" : lexer xs
lexer ( 'D' : 'o' : xs ) = Symbol "Do" : lexer xs
lexer ( 'W' : 'h' : 'i' : 'l' : 'e' : xs ) = Symbol "While" : lexer xs
lexer ( 'E' : 'n' : 'd' : xs ) = Symbol "End" : lexer xs
lexer ( '(' : xs ) = Symbol "(" : lexer xs 
lexer ( ')' : xs ) = Symbol ")" : lexer xs
lexer ( ' ' : xs ) = lexer xs
lexer t@( x : xs ) 
   | isDigit x =  let ( as , bs ) = span isDigit t in Number ( read as :: Int ) : lexer bs
   | otherwise = let ( as , bs ) = break isSpace t in Ident as : lexer bs


type Parser a = [ Token ] -> Maybe ( Command, [ Token ] )


