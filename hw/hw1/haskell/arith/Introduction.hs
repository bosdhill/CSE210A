-- https://ravichugh.gitbooks.io/a-quarter-of-haskell/
-- Introduction
fullName :: [String]
fullName = firstName : lastName : [] where
            firstName = "Bobby"
            lastName = "Dhillon"
absVal n
    | n >= 100  = 100
    | n >= 0    = n
    | otherwise = -n

-- Lists
-- len [a]
-- length of a list
len x
    | x == [] = 0
    | otherwise = 1 + len (tail x)

-- filter
-- flt (2 <) [1,2,3,4]
flt p [] = []
flt p (x:xs)
    | p x = x : flt p xs
    | otherwise = flt p xs

-- map
-- map ((*) 2) [1,2,3,4]
mp f [] = []
mp f (x:xs) = f x : mp f xs

-- repeat n
-- repeat n forever as list
rpt n = n : rpt n

-- take
-- take n from list
tk n [] = []
tk n (x:xs)
    | n > 0 = x : tk (n - 1) xs
    | otherwise = xs

-- Algebraic Datatypes (ADTs)
data Currency
    = USD Float
    | JPY Float
    deriving (Eq, Show)
-- deriving implements the following:
    -- show :: Currency -> String
    -- (==) :: Currency -> Currency -> Bool
yenPerDollar  = 112.86         -- as of 9/27/17
dollarsPerYen = 1 / yenPerDollar

add                    :: Currency -> Currency -> Currency
add (USD d1) (USD d2)  = USD (d1 + d2)
add (JPY d1) (JPY d2)  = JPY (d1 + d2)
add (USD d1) (JPY d2)  = USD (d1 + d2 * dollarsPerYen)
add (JPY d1) (USD d2)  = JPY (d1 + d2 * yenPerDollar)

-- Data Constructors
-- contains one or more data constructors with cap letters
-- type must also have a cap
-- where each data constructor wraps 0 or more values
-- Data Constuctors == functions applied to wrapped type proudce wrapping type

-- Pattern Matching
-- used to match and destruct values of a datatype, can use _
-- at runtime, a variable matching pattern for the data type S
-- matches any value of type S (i.e. Currency), and a data constructor
-- pattern matches D p0 ... pn of type S whose n + 1 values match p0...pn
data T
  = A
  | B Int
  | C Bool String
  | D (Bool, String)
      deriving (Eq, Show)

-- Instead of defining separate functions, do case!
-- foo             :: T -> Int
-- foo A           =  0
-- foo (B i)       =  1
-- foo (C b s)     =  2
-- foo (D (b, s))  =  3

-- Case Expressions
-- foo defines multiple equations
-- can be deconstructed with case expression
-- handles all values of type t
-- THIS CAN BE USED TO DECONSTRUCT Exp
-- But Maps only to number, maybe if uses maybe keyword?
foo :: T -> Int
foo t =
    case t of
        A        -> 0
        B i      -> 1
        C b s    -> 2
        D (b, s) -> 3

-- Polymorphic ADTs
--- undefined has EVERY type
--- undefined is used to RAISE EXCEPTIONS
--- useful for PLACEHOLDERS
data Exception
    = NewException
    | Exception String
    deriving (Eq, Show)
throws (NewException) = undefined
-- better way to represent failure:
--
-- data Maybe a
--     = Nothing
--     | Just a
--     deriving (Eq, Show)
--
-- Maybe is polymoprhic, takes any type
-- Maybe can be either Nothing or Just with the value of type a
-- If Nothing is returned, it can be handled by caller
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x
--- If return type is Maybe, it must return a data constructor of
--- type Maybe, otherwise function will error out!

-- Type Aliases vs. Wrapper Types
-- Define type aliases to avoid confusion when data constructors
-- take similar types (just Synonyms)
type Dollars = Float
type Yen = Float
--- and then wrap these in the data constructor
-- data Currency
--   = USD Dollars -- US Dollars
--   | JPY Yen     -- Japanese Yen
--   deriving (Eq, Show)

-- newtype
--- can be used to define datatypes with one constructor that wraps
-- just one value
newtype Box a = Box a deriving (Eq, Show)

-- Record Types
-- When data constructors carry multiple values, it can be hard to remember
-- which components are meant to represent what (especially when multiple
-- components have the same types). Furthermore, it can be tedious to write
-- accessor functions that retrieve particular components carried
-- by a data constructor.
-- CAN USE RECORD SYNTAX to give names to data values
data ABCD
  = X { bar :: String, baz :: Int }
  | Y { bar :: String, raz :: () }
  | Z Int
  | W
-- and now can construct values like so:
-- let xyz = X { bar = "hello", baz = 17 }
-- which also generates accessor functions bar and baz
-- > (bar xyz)
-- "hello"

-- FUNCTIONS
-- lambdas
-- flipping order of function arguments with sections
-- eta-reduction -> simplify function definition
squareList xs = map (\x -> x ^ 2) xs -- lambda
squareList' xs = map (flip (^) 2) xs -- flip
squareList'' xs = map (^2) xs        -- section
squareList'''   = map (^2)           -- eta-reduction




