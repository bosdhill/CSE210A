-- ref: https://stackoverflow.com/a/16811995
module StateMap where
import Control.Monad.State
import qualified Data.Map as Map

funcs :: Map.Map String Double
funcs = Map.empty

putKeyVal :: String -> Double -> State (Map.Map String Double) ()
putKeyVal str d = do
  funcs <- get
  put (Map.insert str d funcs)

getVal :: String -> State (Map.Map String Double) String
getVal str = do
  funcs <- get
  if (Map.lookup str funcs) == Nothing then return "not defined" else return "ok"

-- main = do
--         putStrLn $ flip evalState funcs $ do { putKeyVal "3" 4.0; getVal "3"}
--         putStrLn $ flip evalState funcs $ do { putKeyVal "4" 2.0; getVal "4"}
--         putStrLn $ flip evalState funcs $ do { putKeyVal "7" 4.0; getVal "8"}