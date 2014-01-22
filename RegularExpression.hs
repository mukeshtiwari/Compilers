{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Control.Applicative hiding ( ( <|> ) , many )
import Data.List ( sort ) 
import Prelude hiding ( foldl )
import qualified Data.Map as Map 

{--
Implementation of Simon Thompson paper 
http://kar.kent.ac.uk/22057/2/Regular_Expressions_and_Automata_using_Haskell.pdf
--}


data Reg = Epsilon
         | Literal Char
         | Or Reg Reg
         | Then Reg Reg
         | Star Reg
         deriving ( Show, Eq )

exprCal = buildExpressionParser table atom where 
   table = [ [ Postfix ( Star <$ char '*' )           ]
            ,[ Infix   ( return  Then ) AssocLeft     ]
            ,[ Infix   ( Or   <$ char '|' ) AssocLeft ]
           ]

   
   lit = noneOf "*|()"
   atom =  char '(' *> exprCal <* char ')'
       <|> ( Literal <$> lit )
       


literals :: Reg -> [ Char ]
literals Epsilon = ""
literals ( Literal ch ) = [ ch ]
literals ( Or r1 r2 ) = literals r1 ++ literals r2 
literals ( Then r1 r2 ) = literals r1 ++ literals r2
literals ( Star r ) = literals r 

-- set implementation. Try to use Data.Set --

data Set a = SetI [ a ] deriving ( Ord, Show ) 

instance ( Eq a ) => Eq ( Set a ) where
  ( SetI a ) == ( SetI b ) = a == b 

empty = SetI []

sing x = SetI [ x ] 

memSet :: Ord a => Set a -> a -> Bool
memSet ( SetI [] ) y  = False
memSet ( SetI ( x : xs ) ) y 
    | x < y = memSet ( SetI xs ) y
    | x == y = True
    | otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union ( SetI xs ) ( SetI ys ) = SetI ( uni xs ys )

uni :: Ord a => [ a ] -> [ a ] -> [ a ]
uni [] ys = ys
uni xs [] = xs
uni ( x : xs ) ( y : ys ) 
   | x < y = x : uni xs ( y : ys )
   | x == y = x : uni xs ys
   | otherwise = y : uni ( x : xs ) ys


inter :: Ord a => Set a -> Set a -> Set a
inter ( SetI xs ) ( SetI ys ) = SetI ( int xs ys )

int :: Ord a => [ a ] -> [ a ] -> [ a ]
int [] ys = []
int xs [] = []
int ( x : xs ) ( y : ys )
   | x < y = int xs ( y : ys ) 
   | x == y = x : int xs ys 
   | otherwise = int ( x : xs ) ys

diff :: Ord a => Set a -> Set a -> Set a
diff ( SetI xs ) ( SetI ys ) = SetI ( dif xs ys )

dif :: Ord a => [ a ] -> [ a ] -> [ a ] 
dif [] ys = []
dif xs [] = xs
dif ( x : xs ) ( y :  ys )  
  | x < y =  x : dif xs ( y : ys )
  | x == y = dif xs ys
  | otherwise = dif ( x : xs ) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet ( SetI xs ) ( SetI ys ) = subS xs ys

subS :: Ord a => [ a ] -> [ a ] -> Bool
subS [] ys = True
subS xs [] = False
subS ( x : xs ) ( y : ys ) 
   | x < y = False
   | x == y = subS xs ys
   | otherwise = subS ( x : xs ) ys


mapSet :: Ord b => ( a -> b ) -> Set a -> Set b
mapSet f ( SetI xs ) = makeSet ( map f xs )

filterSet :: ( a -> Bool ) -> Set a -> Set a
filterSet p ( SetI xs ) = SetI ( filter p xs )

foldSet :: ( a -> a -> a ) -> a -> Set a -> a
foldSet f x ( SetI xs ) = foldr f x xs

makeSet :: Ord a => [ a ] -> Set a
makeSet = SetI . remDups . sort where 
       remDups []  = []
       remDups [ x ]  = [ x ]
       remDups ( x : y : ys ) 
         | x < y = x : remDups ( y : ys )
         | otherwise = remDups ( y : ys )

showSet :: Show a => Set a -> String
showSet ( SetI xs ) = concat ( map ( ( ++ "\n" ) . show ) xs )

card :: Set a -> Int 
card ( SetI xs ) = length xs

flatten :: Set a -> [ a ]
flatten ( SetI xs ) = xs 

setlimit :: Eq a => ( Set a -> Set a ) -> Set a -> Set a
setlimit f s
    | s == next = s 
    | otherwise = setlimit f next where 
         next = f s

--End of set 
 

-- states, moves, starting state and accepting state

data Nfa a = NFA ( Set a ) ( Set ( Move a ) ) a ( Set a ) deriving ( Eq, Show )

data Move a = Move a Char a 
            | EMove a a 
            deriving ( Eq, Ord, Show )

foldl :: ( Set a -> Char -> Set a ) -> Set a -> String -> Set a
foldl f r [] = r
foldl f r ( c : cs ) = foldl f ( f r c ) cs 


trans :: ( Ord a ) => Nfa a -> String -> Set a
trans mach str = foldl step startset str where 
          step set ch = onetrans mach ch set 
          startset = closure mach ( sing ( startstate mach ) )

onetrans :: ( Ord a ) => Nfa a -> Char -> Set a -> Set a 
onetrans mach c x = closure mach ( onemove mach c x )

onemove :: ( Ord a ) => Nfa a -> Char -> Set a -> Set a
onemove ( NFA states moves start term ) c x = 
     makeSet [ s | t <- flatten x , 
                    Move z d s <- flatten moves ,
                    z == t , 
                    c == d ]



closure :: Ord a => Nfa a -> Set a -> Set a 
closure ( NFA states moves start term ) = setlimit add where 
          add stateset = union stateset ( makeSet  accessible ) where 
            accessible = [ s | x <- flatten stateset , 
                               EMove y s <- flatten  moves , 
                               y == x ]
 


startstate (NFA _ _ start _) = start

renumber :: Int -> Int -> Int
renumber n k = n+k

renumber_move :: Int -> Move Int -> Move Int
renumber_move n ( Move s1 ch s2 )  = Move ( s1 + n ) ch ( s2 + n )
renumber_move n ( EMove s1 s2 )    = EMove ( s1 + n ) ( s2 + n )


build :: Reg -> Nfa Int 
build ( Literal c ) = NFA ( makeSet [ 0 .. 1 ] ) ( sing ( Move 0 c 1 ) ) 0 ( sing 1 )
build ( Or r1 r2 ) = mor ( build r1 ) ( build r2 )
build ( Then r1 r2 ) = mthen ( build r1 ) ( build r2 )
build ( Star r ) = mstar ( build r )


mor :: Nfa Int -> Nfa Int -> Nfa Int 
mor ( NFA states1 moves1 start1 finish1 ) ( NFA states2 moves2 start2 finish2 ) = 
    NFA 
        ( states1' `union` states2' `union` newstates ) 
        ( moves1' `union` moves2' `union` newmoves    ) 
        0 
        ( sing ( m1 + m2 + 1 ) )
      where
        m1        = card states1
        m2        = card states2
        states1'  = mapSet ( renumber 1 ) states1
        states2'  = mapSet ( renumber ( m1 + 1 ) ) states2
        newstates = makeSet [ 0 , m1 + m2 + 1 ]
        moves1'   = mapSet ( renumber_move 1 ) moves1
        moves2'   = mapSet ( renumber_move ( m1 + 1 ) ) moves2
        newmoves  = makeSet [ EMove 0 1 , 
                              EMove 0 ( m1 + 1 ) , 
                              EMove m1 ( m1 + m2 + 1 ) , 
                              EMove ( m1 + m2 ) ( m1 + m2 + 1 ) ]


mthen :: Nfa Int -> Nfa Int -> Nfa Int 
mthen ( NFA states1 moves1 start1 finish1 ) ( NFA states2 moves2 start2 finish2 ) = 
   NFA  
      ( union states1 states2' ) 
      ( union moves1 moves2'   ) 
      start1 
      finish2' where
        k         = card states1 - 1
        states2'  = mapSet ( renumber k ) states2
        moves2'   = mapSet ( renumber_move k ) moves2
        finish2'  = mapSet ( renumber k ) finish2


mstar :: Nfa Int -> Nfa Int 
mstar  ( NFA states moves start finish ) = 
  NFA ( states' `union` newstates ) 
      ( moves' `union` newmoves ) 
      0 
      ( sing ( m + 1 ) ) where
         m         = card states
         states'   = mapSet ( renumber 1 ) states
         newstates = makeSet [ 0 , m+1 ]
         moves'    = mapSet ( renumber_move 1 ) moves
         newmoves  = makeSet [ EMove 0 1, 
                               EMove m 1, 
                               EMove 0 ( m + 1 ), 
                               EMove m ( m + 1 ) ]


-- end of NFA 

--start of dfs

dfs :: Char -> Int -> Int -> Map.Map (Int, Int) (Int,Char) -> [Move Int ] -> Map.Map (Int, Int) (Int, Char)
dfs p len cur par moves
  | len > 100 = par
  | otherwise  =  ans2 where 
        edges1 = [ ( s , ch  , d ) | Move s ch d <- moves , s == cur  ]
        edges2 = [ ( s , '$' , d ) | EMove s d <- moves , s == cur ]
        ans1  = 
           foldr  ( \(s , ch , d ) acc-> 
               if Map.member ( d , len+1 ) acc then acc 
               else dfs ch (len+1) d 
                   ( Map.insert ( d , len+1 ) ( s , ch ) acc ) moves ) par edges1
        ans2 = 
          foldr (\(s,ch,d) acc->
             if Map.member ( d ,len ) acc then acc 
             else dfs ch len d (Map.insert (d,len) (s,ch) acc) moves) ans1 edges2
      
       

path :: Int -> Int -> Map.Map (Int, Int) (Int, Char) -> [Char]
path node len par
  | len == 0 = []
  | otherwise = case Map.lookup (node, len) par of
                  Nothing       ->  []
                  Just (s, '$') ->  path s len par
                  Just (s, ch)  ->  ch: path s (len-1) par

solve :: Int -> Reg -> String
solve len reg = reverse p where 
    NFA _ (SetI moves) s ( SetI [ d  ] ) = reg `seq` build reg
    par =  dfs '$' 0 s Map.empty moves
    p = path d len par
 

--end of dfs

   
main = do
    n <- ( read :: String -> Int )  <$> getLine
    expr <- getLine
    case parse exprCal "" expr of
             Left msg -> error "failed to parse"
             Right val -> putStrLn ( solve n val )   


{--
generate :: Reg ->  [ String ] 
generate Epsilon  =  [ "" ]
generate ( Literal ch )  =   [ [ ch ] ]
generate ( Or r1 r2 )  = 
                      let 
                        ( x : xs ) = generate r1 
                        ( y : ys ) = generate r2  
                      in  x : y : infiniStream xs ys 
generate ( Then r1 r2 )  = [ xs ++ ys | xs <- generate r1 , ys <- generate r2 ]
generate ( Star r )  = concat[ ( iterate ( ++ xs ) xs ) | xs <- generate r ]
      

infiniStream :: [ String ] -> [ String ] -> [ String ]
infiniStream [] ys = ys
infiniStream xs [] = xs
infiniStream ( x : xs ) ( y : ys ) = x : y : infiniStream xs ys 

--}



