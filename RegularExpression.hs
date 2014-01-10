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
   table = [ [ Postfix (  Star <$ char '*' ) ]
            ,[ Infix ( return  sequence) AssocLeft ]
            ,[ Infix ( choice <$ char '|' ) AssocLeft ]
           ]

   
   lit = noneOf "*|()"
   sequence a b = Then  (seqTerms a)  (seqTerms b)
   choice a b = Or  (choiceTerms a)  (choiceTerms b)
   seqTerms (Then ts ps ) = Then ( seqTerms ts ) ( seqTerms ps )
   seqTerms t = t
   choiceTerms (Or ts ps ) = Or ( choiceTerms ts ) ( choiceTerms ps )
   choiceTerms t =  t
   atom =  char '(' *> exprCal <* char ')'
       <|> ( Literal <$> lit )
       

main = parseTest exprCal "a*b*c*"
