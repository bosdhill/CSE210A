module Expressions where

import Data.Char
import System.Environment

data Token
  = PlusTok
  | TimesTok
  | OpenTok
  | CloseTok
  | IntTok Int
  deriving (Show)

data Expr
  = IntLit Int          -- integer constants, leaves of the expression trees
  | Add    Expr Expr    -- summation node
  | Mult   Expr Expr    -- multiplication node
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('(' : restStr) = OpenTok  : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer (chr : restStr) | (isSpace chr) = lexer restStr
lexer str@(chr : restStr) | (isDigit chr)
  = IntTok (stringToInt digitStr) : lexer restStr'
  where
     (digitStr, restStr') = break (not. isDigit) str
     -- defining a local function here:
     stringToInt :: String -> Int
     stringToInt  = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
     -- runtime error for all other characters:
lexer (chr :restString)
  = error ("lexer: unexpected character: " ++ (show chr))

parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLit n,   restTokens)
parseIntOrParenExpr (OpenTok : restTokens1)
  = case parseSumOrProdOrIntOrParenExpr restTokens1 of
       Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
       Just _  -> Nothing -- no closing paren
       Nothing -> Nothing
parseIntOrParenExpr tokens
  = Nothing

{-
parseProdOrIntOrParenExpr tokens =
  case parseProdOrIntOrParenExprL tokens of
    Just (exprs, restToken) -> Just (foldl1 Mult exprs, restToken)
    Nothing -> Nothing
-}

parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parseIntOrParenExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) ->
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parseProdOrIntOrParenExprL :: [Token] -> Maybe ([Expr], [Token])
parseProdOrIntOrParenExprL tokens
  = case parseIntOrParenExpr tokens of
      Just (expr, (TimesTok : restTokens1)) ->
          case parseProdOrIntOrParenExprL restTokens1 of
            Just (exprs, restTokens2) -> Just (expr:exprs, restTokens2)
            Nothing                   -> Nothing
      Just (expr, restToken)   -> Just ([expr], restToken)
      Nothing                  -> Nothing

parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) ->
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr
    _                    -> error "Could not parse input"

eval :: Expr -> Int
eval (IntLit n) = n
eval (Add expr1 expr2)
  = eval expr1 + eval expr2
eval (Mult expr1 expr2)
  = eval expr1 * eval expr2

main = do
    let y = parse lexer "3 + 4"
    putStrLn(y)


