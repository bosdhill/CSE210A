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
    let stmt = filter (\x -> not (any (x ==) "\n\r\t")) stmt'
    let state = M.fromList []
    let m = eval state (parseString stmt)
    let f result k a = case result of
            "{"       -> result ++ k ++ " → " ++ (show a)
            otherwise -> result ++ ", " ++ k ++ " → " ++ (show a) in
            putStrLn ((M.foldlWithKey f "{" m) ++ "}")
