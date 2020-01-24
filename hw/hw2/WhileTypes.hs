{---------------------
Program: WhileTypes.hs
Author: Bobby Dhillon
ref: https://wiki.haskell.org/Parsing_a_simple_imperative_language?fbclid=IwAR2cvuYf6YlGhJNaTK6SGwqGk24GJY2Wc5IEG1p4OrBIgsOAzPg5ZGMLTDE
----------------------
-}
module WhileTypes where

data BExpr = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less | Equal deriving (Show)

data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | ABinary ABinOp AExpr AExpr
            deriving (Show)

data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Show)

data Stmt = Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
            deriving (Show)