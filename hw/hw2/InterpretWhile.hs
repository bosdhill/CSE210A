module Main where
import ParseWhile
import WhileTypes
import StateMap
import Control.Monad.State
-- https://stackoverflow.com/questions/22624924/how-do-i-print-the-name-and-value-of-a-custom-data-type-in-haskell
-- What are the return types of a while program?
-- Maybe error, IO(), etc.

eval_abinop :: ABinOp -> AExpr -> AExpr -> Integer
eval_abinop op a1 a2 =
    case op of
        Add      -> (eval_aexpr a1) + (eval_aexpr a2)
        Subtract -> (eval_aexpr a1) - (eval_aexpr a2)
        Multiply -> (eval_aexpr a1) * (eval_aexpr a2)
        -- Divide   -> (eval_aexpr a1) / (eval_aexpr a2)

eval_aexpr :: AExpr -> Integer
eval_aexpr exp =
    case exp of
        Var s            -> undefined -- hashmap
        IntConst c       -> c
        Neg aExp         -> eval_aexpr aExp
        ABinary op a1 a2 -> eval_abinop op a1 a2

eval_rbinary :: RBinOp -> AExpr -> AExpr -> Bool
eval_rbinary op a1 a2 =
    case op of
        Greater -> (eval_aexpr a1) > (eval_aexpr a2)
        Less    -> (eval_aexpr a1) < (eval_aexpr a2)

eval_bbinary :: BBinOp -> BExpr -> BExpr -> Bool
eval_bbinary op b1 b2 =
    case op of
        And -> (eval_bexpr b1) && (eval_bexpr b2)
        Or  -> (eval_bexpr b1) || (eval_bexpr b2)

eval_bexpr :: BExpr -> Bool
eval_bexpr a =
    case a of
        BoolConst t   -> t
        Not t         -> not (eval_bexpr t)
        BBinary a b c -> eval_bbinary a b c
        RBinary a b c -> eval_rbinary a b c

eval_while :: BExpr -> Stmt -> IO()
eval_while a b
    | eval_bexpr a = eval b
    | otherwise    = undefined

-- eval_assign :: String -> AExpr
-- eval_assign var val =

eval :: Stmt -> IO()
eval s =
    case s of
        Assign a b -> undefined
        If a b c   -> undefined
        While a b  -> eval_while a b
        Skip       -> return ()

main :: IO ()
main = eval (parseString ("x := 1; x := 1; while x < 3 do skip"))