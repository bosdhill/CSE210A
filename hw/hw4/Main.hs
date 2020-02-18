{---------------------
Program: Main.hs
Author: Bobby Dhillon
----------------------
-}
module Main where
import InterpretWhile
import ParseWhile
import WhileTypes
import qualified Data.Map as M

main :: IO ()
main = do
    stmt' <- getContents
    let stmt = parseString $ filter (\x -> not (any (x ==) "\n\r\t")) stmt'
    let state = M.fromList []
    let steps = ([])
    putStrLn (printSteps (eval1 state stmt steps))
