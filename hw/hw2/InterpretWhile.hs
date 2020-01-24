{---------------------
Program: InterpretWhile.hs
Author: Bobby Dhillon
ref: https://stackoverflow.com/questions/27660791/insert-value-into-a-map-and-then-return-it
----------------------
-}
module InterpretWhile where
import ParseWhile
import WhileTypes
import qualified Data.Map as M

lookupState :: M.Map String Integer -> String -> Integer
lookupState state k =
    case M.lookup k state of
        Nothing  -> 0
        Just val -> val

eval_abinop :: M.Map String Integer -> ABinOp -> AExpr -> AExpr -> Integer
eval_abinop state op a1 a2 =
    case op of
        Add      -> (eval_aexpr state a1) + (eval_aexpr state a2)
        Subtract -> (eval_aexpr state a1) - (eval_aexpr state a2)
        Multiply -> (eval_aexpr state a1) * (eval_aexpr state a2)
        -- Divide   -> (eval_aexpr a1) / (eval_aexpr a2)

eval_aexpr :: M.Map String Integer -> AExpr -> Integer
eval_aexpr state exp =
    case exp of
        Var s            -> lookupState state s
        IntConst c       -> c
        Neg aExp         -> - eval_aexpr state aExp
        ABinary op a1 a2 -> eval_abinop state op a1 a2

eval_rbinary :: M.Map String Integer -> RBinOp -> AExpr -> AExpr -> Bool
eval_rbinary state op a1 a2 =
    case op of
        Greater -> (eval_aexpr state a1) > (eval_aexpr state a2)
        Less    -> (eval_aexpr state a1) < (eval_aexpr state a2)
        Equal   -> (eval_aexpr state a1) == (eval_aexpr state a2)

eval_bbinary :: M.Map String Integer -> BBinOp -> BExpr -> BExpr -> Bool
eval_bbinary state op b1 b2 =
    case op of
        And   -> (eval_bexpr state b1) && (eval_bexpr state b2)
        Or    -> (eval_bexpr state b1) || (eval_bexpr state b2)

eval_bexpr :: M.Map String Integer -> BExpr -> Bool
eval_bexpr state a =
    case a of
        BoolConst t   -> t
        Not t         -> not (eval_bexpr state t)
        BBinary a b c -> eval_bbinary state a b c
        RBinary a b c -> eval_rbinary state a b c

eval_while :: M.Map String Integer -> BExpr -> Stmt -> M.Map String Integer
eval_while state a b
    | eval_bexpr state a = eval (eval state b) (While a b)
    | otherwise          = state

eval_assign :: M.Map String Integer -> String -> AExpr -> M.Map String Integer
eval_assign state k v = M.insert k (eval_aexpr state v) state

eval_if :: M.Map String Integer -> BExpr -> Stmt -> Stmt -> M.Map String Integer
eval_if state bexpr s1 s2
    | eval_bexpr state bexpr = eval state s1
    | otherwise              = eval state s2

eval :: M.Map String Integer -> Stmt -> M.Map String Integer
eval state s =
    case s of
        Seq []     -> state
        Seq x      -> eval (eval state $ head x) $ Seq $ tail x
        Assign a b -> eval_assign state a b
        If a b c   -> eval_if state a b c
        While a b  -> eval_while state a b
        Skip       -> state