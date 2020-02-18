{---------------------
Program: WhileTypes.hs
Author: Bobby Dhillon
ref: https://wiki.haskell.org/Parsing_a_simple_imperative_language?fbclid=IwAR2cvuYf6YlGhJNaTK6SGwqGk24GJY2Wc5IEG1p4OrBIgsOAzPg5ZGMLTDE
----------------------
-}
module WhileTypes where
import Data.List
import qualified Data.Map as M

type State = M.Map String Integer
type Step = (Stmt, State)
type Steps = [(Stmt, State)]

data BExpr = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr

data BBinOp = And | Or

data RBinOp = Greater | Less | Equal

data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | ABinary ABinOp AExpr AExpr

data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
             | Exp

data Stmt = Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip

formatter result k a = case result of
        "{"       -> result ++ k ++ " → " ++ (show a)
        otherwise -> result ++ ", " ++ k ++ " → " ++ (show a)

printMap :: State -> String
printMap m = (M.foldlWithKey formatter "{" m) ++ "}"

printSteps :: Steps -> String
printSteps steps =
    case steps of
        (s, m):xs  -> if m == M.empty
                      then
                        "⇒ " ++ show s ++ ", {}\n" ++ (printSteps xs)
                      else
                        "⇒ " ++ show s ++ ", " ++ (printMap m) ++ "\n" ++ (printSteps xs)
        []         -> ""

instance Show AExpr where
  show (IntConst n) = show n
  show (Var s) = s
  show (Neg a) = "-" ++ show a
  show (ABinary Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (ABinary Subtract a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (ABinary Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

instance Show BExpr where
  show (BoolConst True) = "true"
  show (BoolConst False) = "false"
  show (RBinary Equal a b) = "(" ++ show a ++ "=" ++ show b ++ ")"
  show (RBinary Less a b) = "(" ++ show a ++ "<" ++ show b ++ ")"
  show (BBinary Or a b) = "(" ++ show a ++ "∨" ++ show b ++ ")"
  show (BBinary And a b) = "(" ++ show a ++ "∧" ++ show b ++ ")"
  show (Not a) = "¬" ++ show a

instance Show Stmt where
  show Skip = "skip"
  show (Assign s a) = s ++ " := " ++ show a
  show (Seq a) = show (head a) ++ "; " ++ show (head (tail a))
  show (If c a b) = "if " ++ show c ++ " then { " ++ show a ++ " } else { " ++ show b ++ " }"
  show (While b c) = "while " ++ show b ++ " do { " ++ show c ++ " }"