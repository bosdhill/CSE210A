{---------------------
Program: arith.hs
Author: Bobby Dhillon
ref: https://wiki.haskell.org/Parsing_a_simple_imperative_language?fbclid=IwAR2cvuYf6YlGhJNaTK6SGwqGk24GJY2Wc5IEG1p4OrBIgsOAzPg5ZGMLTDE
     https://classes.soe.ucsc.edu/cse210a/Winter20/02-arith-bigstep.pdf
----------------------
-}
module Main where
import Data.Either
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Exp = IntExp Integer
            | SumExp Exp Exp
            | MulExp Exp Exp
            | ExExp Exp Exp
            deriving (Show, Eq)

eval :: Exp -> Integer
eval (IntExp n)     = n
eval (SumExp e1 e2) = (eval e1) + (eval e2)
eval (MulExp e1 e2) = (eval e1) * (eval e2)
eval (ExExp e1 e2)  = (eval e1) ^ (eval e2)

arithDef = emptyDef { Token.reservedOpNames = ["+", "-", "^"] }
lexer = Token.makeTokenParser arithDef

integer    = Token.integer    lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
parens     = Token.parens     lexer

arithParser :: Parser Exp
arithParser = whiteSpace >> aExp

aExp :: Parser Exp
aExp = buildExpressionParser aOperators aTerm

aOperators = [  [Infix  (reservedOp "^"   >> return ExExp) AssocLeft]
              , [Infix  (reservedOp "*"   >> return MulExp) AssocLeft]
              , [Infix  (reservedOp "+"   >> return SumExp) AssocLeft]
               ]

aTerm =  parens aExp
     <|> liftM IntExp integer

parser :: String -> Exp
parser str =
    let s = "(" ++ str ++ ")" in
        case parse arithParser "" s of
            Left e  -> error $ show e
            Right r -> r

main = do
  line <- getLine
  print (eval (parser line))

test_eval1 :: Bool
test_eval1 = let e = MulExp (SumExp (IntExp 3) (IntExp 5)) (IntExp 2) in
                    (eval e) == 16 -- True

test_eval2 :: Bool
test_eval2 = let e = ExExp (IntExp 2) (MulExp (IntExp 3) (IntExp 1)) in
                    (eval e) == 8 -- True

test_parse1 :: Bool
test_parse1 = let e = "3 + 4 ^ 2" in (eval (parser e)) == 19 -- True

test_parse2 :: Bool
test_parse2 = let e = "- 3 + 4 ^ 2" in (eval (parser e)) == 13 -- True