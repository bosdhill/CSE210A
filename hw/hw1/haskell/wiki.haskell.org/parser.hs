module ParseArith where
import Data.Either
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec (try)
import Text.ParserCombinators.Parsec.Char (oneOf, char, digit, satisfy)
import Text.ParserCombinators.Parsec.Combinator (many1, choice, chainl1)

data Exp = IntExp Integer
            | SumExp Exp Exp
            | MulExp Exp Exp
            deriving (Show, Eq)

eval :: Exp -> Integer
eval (IntExp n)     = n
eval (SumExp e1 e2) = (eval e1) + (eval e2)
eval (MulExp e1 e2) = (eval e1) * (eval e2)

test_eval :: Bool
test_eval = let e = MulExp (SumExp (IntExp 3) (IntExp 5))
                           (IntExp 2) in
            (eval e) == 16 -- True

arithDef =
   emptyDef { Token.reservedOpNames = ["+", "-"
                                      ]
            }

lexer = Token.makeTokenParser arithDef

integer    = Token.integer    lexer -- parses an integer
reservedOp = Token.reservedOp lexer -- parses an operator
whiteSpace = Token.whiteSpace lexer -- parses whitespace

arithParser :: Parser Exp
arithParser = whiteSpace >> aExp

aExp :: Parser Exp
aExp = buildExpressionParser aOperators aTerm

aOperators = [ [Infix  (reservedOp "*"   >> return MulExp) AssocLeft]
              , [Infix  (reservedOp "+"   >> return SumExp) AssocLeft]
               ]

aTerm = undefined
    --     liftM IntExp integer
    --  <|> do
    --         a1 <- aExp
    --         op <- aOperators
    --         a2 <- aExp
    --         return op a1 a2

parseString :: String -> Exp
parseString str =
   case parse arithParser "" str of
     Left e  -> error $ show e
     Right r -> r

parens :: Parser Exp
parens = do
    e <- many1 digit
    return (IntExp (read e))

parseStr :: String -> Integer
parseStr str =
   case parse parens "" str of
     Left e  -> error $ show e
     Right r -> eval r