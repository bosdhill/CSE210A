-- {-# LANGUAGE BlockArguments #-}
import Text.ParserCombinators.ReadP
import Data.Char

data Exp = IntExp Int
            | SumExp Exp Exp
            | MulExp Exp Exp
            deriving (Show, Eq) --- value will be printable and comparable

eval :: Exp -> Int
eval (IntExp n)     = n
eval (SumExp e1 e2) = (eval e1) + (eval e2)
eval (MulExp e1 e2) = (eval e1) * (eval e2)

parser:: String -> Maybe (String, a) --- Maybe -> may fail in parsing
parser = undefined

isMul :: Char -> Bool
isMul char =
    any (char ==) "*"

isAdd :: Char -> Bool
isAdd char =
    any (char ==) "+"

parseMul :: ReadP Char
parseMul =
    satisfy isMul

parseAdd :: ReadP Char
parseAdd =
    satisfy isAdd

parseInt :: ReadP Char
parseInt =
    satisfy isDigit

atLeastOneMul :: ReadP [Char]
atLeastOneMul =
    many1 parseMul

atLeastOneAdd :: ReadP [Char]
atLeastOneAdd =
    many1 parseAdd

atLeastOneInt :: ReadP [Char]
atLeastOneInt =
    many1 parseInt

test_eval :: Bool
test_eval = let e = MulExp (SumExp (IntExp 3) (IntExp 5))
                           (IntExp 2) in
            (eval e) == 16 -- True
test_eval2 :: Bool
test_eval2 = let e = IntExp 3 `SumExp` IntExp 5 in
            (eval e) == 8 -- True

--- parser with https://en.wikibooks.org/wiki/Haskell/ParseExps ---

-- data Tree = Branch Tree Tree | Leaf deriving Show

-- leaf = do
--         char 'o'
--         return Leaf

-- -- num = do
-- --         any (char ==) ['0'..'9']
-- --         return Branch

-- brackets p = do
--                 char '('
--                 r <- p
--                 char ')'
--                 return r

-- branch = do a <- leaf -- +++ brackets tree
--             char '&'
--             b <- tree
--             return (Branch a b)

-- tree = leaf +++ branch -- +++ brackets tree

-- ref: https://en.wikibooks.org/wiki/Haskell/ParseExps
brackets p = do char '('
                r <- p
                char ')'
                return r

data Operator = Add | Mul | Exp deriving Show
operators = [(Add,'+'),(Mul,'*'),(Exp,'^')]

data Tree = Branch Operator Tree Tree | Leaf String deriving Show

leaf = do
        s <- many1 (choice (map char ['0'..'9']))
        return (Leaf s)

tree = foldr (\(op,name) p ->
                let this = p +++ do
                                    a <- p +++ brackets tree
                                    char name
                                    b <- this
                                    return (Branch op a b)
                in this)
            (leaf +++ brackets tree)
            operators

-- ref: https://en.wikibooks.org/wiki/Haskell/ParseExps
brackets p = do char '('
                r <- p
                char ')'
                return r

data Operator = Add | Mul | Exp deriving Show
operators = [(Add,'+'),(Mul,'*'),(Exp,'^')]

data Tree = Branch Operator Tree Tree | Leaf String deriving Show

leaf = do
        s <- many1 (choice (map char ['0'..'9']))
        return (Leaf s)

tree = foldr (\(op,name) p ->
                let this = p +++ do
                                    a <- p +++ brackets tree
                                    char name
                                    b <- this
                                    return (Branch op a b)
                in this)
            (leaf +++ brackets tree)
            operators




