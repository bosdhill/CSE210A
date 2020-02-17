{---------------------
Program: ParseWhile.hs
Author: Bobby Dhillon
ref: https://wiki.haskell.org/Parsing_a_simple_imperative_language?fbclid=IwAR2cvuYf6YlGhJNaTK6SGwqGk24GJY2Wc5IEG1p4OrBIgsOAzPg5ZGMLTDE
----------------------
-}
module ParseWhile where
import WhileTypes
import Data.Either
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
   emptyDef { Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.opLetter        = oneOf "=" -- thanks to Michael C.
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "¬"
                                      , "∧"
                                      , "∨"
                                      , "="
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "="
                                      , "<", ">", "∧", "∨", "¬", "^"
                                      ]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
parens     = Token.parens     lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =  braces statement
         <|> sequenceOfStmt

sequenceOfStmt = do
      list <- (sepBy1 statement' semi)
      return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
            <|> whileStmt
            <|> skipStmt
            <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
   do reserved "if"
      cond  <- bExpression
      reserved "then"
      stmt1 <- statement
      reserved "else"
      stmt2 <- statement
      return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
   do reserved "while"
      cond <- bExpression
      reserved "do"
      stmt <- statement
      return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
   do var  <- identifier
      reservedOp ":="
      expr <- aExpression
      return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ],
                [Infix  (reservedOp "^"   >> return (ABinary Exp     )) AssocLeft]
              , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                 Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                 Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
               ]

bOperators = [ [Prefix (reservedOp "¬" >> return (Not             ))          ]
              , [Infix  (reservedOp "∧" >> return (BBinary And     )) AssocLeft,
                 Infix  (reservedOp "∨"  >> return (BBinary Or      )) AssocLeft]
              ]

aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM IntConst integer

bTerm =   parens bExpression
      <|> (reserved "true"  >> return (BoolConst True ))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression

rExpression =
   do a1 <-    aExpression
         <|>   parens aExpression
      op <- relation
      a2 <-    aExpression
         <|>   parens aExpression
      return $ RBinary op a1 a2

relation =  (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)
        <|> (reservedOp "=" >> return Equal)

parseString :: String -> Stmt
parseString str =
   case parse whileParser "" str of
     Left e  -> error $ show e
     Right r -> r