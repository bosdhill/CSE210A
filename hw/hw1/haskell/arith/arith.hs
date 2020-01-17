-- ref: https://wiki.haskell.org/Parsing_a_simple_imperative_language?fbclid=IwAR2cvuYf6YlGhJNaTK6SGwqGk24GJY2Wc5IEG1p4OrBIgsOAzPg5ZGMLTDE
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

parser :: String -> Integer
parser str =
    let s = "(" ++ str ++ ")" in
        case parse arithParser "" s of
            Left e  -> error $ show e
            Right r -> eval r

main = do
  line <- getLine
  print (parser line)