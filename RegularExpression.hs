import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Control.Applicative hiding ( ( <|> ) , many )


data Reg = Epsilon
         | Literal Char
         | Or Reg Reg
         | Then Reg Reg
         | Star Reg
         deriving ( Show, Eq )

exprCal = buildExpressionParser table atom where 
   table = [ [ Postfix ( Star <$ char '*' )           ]
            ,[ Infix   ( return  sequence ) AssocLeft  ]
            ,[ Infix   ( Or   <$ char '|' ) AssocLeft ]
           ]

   
   lit = noneOf "*|()"
   sequence a b = Then  (seqTerms a)  (seqTerms b)
   seqTerms (Then ts ps ) = Then ( seqTerms ts ) ( seqTerms ps )
   seqTerms t = t
   atom =  char '(' *> exprCal <* char ')'
       <|> ( Literal <$> lit )
       

main = parseTest exprCal "a*b*c*"
