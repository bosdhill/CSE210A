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

lookupState :: State -> String -> Integer
lookupState state k =
    case M.lookup k state of
        Nothing  -> 0
        Just val -> val

eval_abinop :: State -> ABinOp -> AExpr -> AExpr -> Integer
eval_abinop state op a1 a2 =
    case op of
        Add      -> (eval_aexpr state a1) + (eval_aexpr state a2)
        Subtract -> (eval_aexpr state a1) - (eval_aexpr state a2)
        Multiply -> (eval_aexpr state a1) * (eval_aexpr state a2)
        Exp      -> (eval_aexpr state a1) ^ (eval_aexpr state a2)

eval_aexpr :: State -> AExpr -> Integer
eval_aexpr state exp =
    case exp of
        Var s            -> lookupState state s
        IntConst c       -> c
        Neg aExp         -> - eval_aexpr state aExp
        ABinary op a1 a2 -> eval_abinop state op a1 a2

eval_rbinary :: State -> RBinOp -> AExpr -> AExpr -> Bool
eval_rbinary state op a1 a2 =
    case op of
        Greater -> (eval_aexpr state a1) > (eval_aexpr state a2)
        Less    -> (eval_aexpr state a1) < (eval_aexpr state a2)
        Equal   -> (eval_aexpr state a1) == (eval_aexpr state a2)

eval_bbinary :: State -> BBinOp -> BExpr -> BExpr -> Bool
eval_bbinary state op b1 b2 =
    case op of
        And   -> (eval_bexpr state b1) && (eval_bexpr state b2)
        Or    -> (eval_bexpr state b1) || (eval_bexpr state b2)

eval_bexpr :: State -> BExpr -> Bool
eval_bexpr state a =
    case a of
        BoolConst t   -> t
        Not t         -> not (eval_bexpr state t)
        BBinary a b c -> eval_bbinary state a b c
        RBinary a b c -> eval_rbinary state a b c

eval_while :: State -> BExpr -> Stmt -> State
eval_while state a b
    | eval_bexpr state a = eval (eval state b) (While a b)
    | otherwise          = state

eval_assign :: State -> String -> AExpr -> State
eval_assign state k v = M.insert k (eval_aexpr state v) state

eval_if :: State -> BExpr -> Stmt -> Stmt -> State
eval_if state bexpr s1 s2
    | eval_bexpr state bexpr = eval state s1
    | otherwise              = eval state s2

eval :: State -> Stmt -> State
eval state s =
    case s of
        Seq []     -> state
        Seq x      -> eval (eval state $ head x) $ Seq $ tail x
        Assign a b -> eval_assign state a b
        If a b c   -> eval_if state a b c
        While a b  -> eval_while state a b
        Skip       -> state

--- also need access to the next state
eval_assign_small :: State -> String -> AExpr -> Step
eval_assign_small state k v =
    do let v' = (eval_aexpr state v)
       let state' = M.insert k v' state
       (Skip, state')

eval_if_small :: State -> BExpr -> Stmt -> Stmt -> Step
eval_if_small state bexpr s1 s2
    | eval_bexpr state bexpr = (s1, state)
    | otherwise              = (s2, state)

eval_seq_small :: State -> Stmt -> Stmt -> Steps -> Steps
eval_seq_small state c1 c2 steps =
    case c1 of
        Skip      -> do let steps' = steps ++ [(c2, state)]
                        eval1 state c2 steps'
        otherwise -> do let steps' = eval1 state c1 steps
                        let (stmt', state') = last steps'
                        let steps'' = steps ++ [(Seq [stmt', c2], state')]
                        eval_seq_small state' stmt' c2 steps''

eval1 :: State -> Stmt -> Steps -> Steps
eval1 state stmt steps =
    case stmt of
        Assign a b -> do let (stmt', state') = eval_assign_small state a b
                         steps ++ [(stmt', state')]
        If a b c   -> do 
                      let (stmt', state') = eval_if_small state a b c
                      let steps' = steps ++ [(stmt', state')]
                      if length steps < 9999 then
                         eval1 state' stmt' steps'
                      else
                         steps'
        While a b  -> eval1 state (If a (Seq [b, (While a b)]) Skip) steps
        Seq a      -> eval_seq_small state (head a) (head (tail a)) steps
        Skip       -> steps

test_assign :: IO()
test_assign =
    do let steps = eval1 (M.fromList []) (Assign "x" (IntConst 3)) ([])
       putStrLn (printSteps steps)

test_if :: IO()
test_if =
    do let steps = eval1 (M.fromList []) (parseString "if x=0 ∧ y < 4 then x:= 1 else x:= 3") ([])
       putStrLn (printSteps steps)

-- test
-- while x=0 do x := 3'

-- his
-- ⇒ x := 3; while (x=0) do { x := 3 }, {}
-- ⇒ skip; while (x=0) do { x := 3 }, {x → 3}
-- ⇒ while (x=0) do { x := 3 }, {x → 3}
-- ⇒ skip, {x → 3}'

-- mine
-- ⇒ x := 3; while (x=0) do { x := 3 }, {}
-- ⇒ skip, {x → 3}
-- ⇒ skip, {x → 3}
test_while :: IO()
test_while =
    do let steps = eval1 (M.fromList []) (parseString "while x=0 do x := 3") ([])
       putStrLn (printSteps steps)

-- '{ a:= 1; b:=2 }; c:=3'
-- his 
-- '⇒ skip; b := 2; c := 3, {a → 1}
-- ⇒ b := 2; c := 3, {a → 1}
-- ⇒ skip; c := 3, {a → 1, b → 2}
-- ⇒ c := 3, {a → 1, b → 2}
-- ⇒ skip, {a → 1, b → 2, c → 3}'

-- mine
-- ⇒ skip; b := 2, {a → 1}
-- ⇒ b := 2, {a → 1}
-- ⇒ skip, {a → 1, b → 2}
test_seq :: IO()
test_seq =
    do let steps = eval1 (M.fromList []) (parseString "{ a:= 1; b:=2 }; c:=3") ([])
       putStrLn (printSteps steps)


-- 'while ¬(x  < 0) do x := -1
-- '⇒ x := -1; while ¬(x<0) do { x := -1 }, {}
-- ⇒ skip; while ¬(x<0) do { x := -1 }, {x → -1}
-- ⇒ while ¬(x<0) do { x := -1 }, {x → -1}
-- ⇒ skip, {x → -1}'

test_while_not_f :: IO()
test_while_not_f =
    do let steps = eval1 (M.fromList []) (parseString "while ¬(x  < 0) do x := -1") ([])
       putStrLn (printSteps steps)

-- test_convert_seq :: IO()
-- test_convert_seq =
--     do let stmt = parseString "x:=1;x:=4;x:=5"
--        putStrLn $ show stmt
--        putStrLn (show (convert_seq stmt))