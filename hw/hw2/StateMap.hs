-- ref: https://stackoverflow.com/a/16811995
module StateMap where
-- import Control.Monad.State
-- import Text.Printf
-- import qualified Data.Map as Map

-- formatPence = printf "%.2f" . (/100) . fromIntegral

-- funcs :: Map.Map String Double
-- funcs = Map.empty

-- putKeyVal :: String -> Double -> State (Map.Map String Double) ()
-- putKeyVal str d = do
--   funcs <- get
--   put (Map.insert str d funcs)

-- getVal :: String -> Double
-- getVal str =
--   let x = (Map.lookup str funcs) in
--     case x of
--       Nothing   -> 0.0
--       otherwise -> funcs Map.! str

-- main = do
--         putStrLn $ flip evalState funcs $ do { putKeyVal "4" 2.0; getVal "4"}
import Control.Monad
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fail
import Control.Monad.Maybe
import Control.Monad.State
import qualified Data.Map as Map

type MapM k v a = MaybeT (State (Map.Map k v)) a

lookupM k = MaybeT $ Map.lookup k `liftM` get
insertM k = modify . Map.insert k
deleteM k = modify $ Map.delete k

runMap m = (flip execState) m . runMaybeT

foo = runMap Map.empty $ do
    insertM 5 20
    v <- lookupM 4
    deleteM v