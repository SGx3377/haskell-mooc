-- Exercise set 7

module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (sort, nub)
import Data.Monoid
import Data.Semigroup


------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)


velocity :: Distance -> Time -> Velocity
velocity (Distance d) (Time t) = Velocity (d / t)
travel :: Velocity -> Time -> Distance
travel (Velocity v) (Time t) = Distance (v * t)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a] deriving (Show, Eq)

emptySet :: Set a
emptySet = Set []

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

add :: Ord a => a -> Set a -> Set a
add x (Set xs) = Set (insertSorted x xs)

insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | x == y = y:ys
  | x < y = x:y:ys
  | otherwise = y : insertSorted x ys


------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

data State = Start | Add | Sweet | Dough | Batter | Mixed | Error | Finished
  deriving (Eq, Show)

step :: State -> Event -> State
step Finished _     = Finished
step Error _        = Error
step Start AddEggs  = Add
step Add AddFlour   = Dough
step Dough AddSugar = Batter
step Add AddSugar   = Sweet
step Sweet AddFlour = Batter
step Batter Mix     = Mixed
step Mixed Bake     = Finished
step _ _            = Error

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

average :: Fractional a => NonEmpty a -> a
average xs = let (total, count) = foldl' (\(accSum, accCount) x -> (accSum + x, accCount + 1)) (0, 0) xs
             in total / fromIntegral count

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.
--
-- PS. The Data.List.NonEmpty type has been imported for you

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) = foldl' (flip (<|)) (pure x) xs


------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  Distance a <> Distance b = Distance (a + b)

instance Semigroup Time where
  Time a <> Time b = Time (a + b)

instance Semigroup Velocity where
  Velocity a <> Velocity b = Velocity (a + b)

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?



data Set1 a = Set1 [a]
  deriving (Show, Eq)

instance (Eq a, Ord a) => Semigroup (Set a) where
  Set xs <> Set ys = Set (merge xs ys)
    where
      merge [] ys = ys
      merge xs [] = xs
      merge (x:xs) (y:ys)
        | x < y     = x : merge xs (y:ys)
        | x > y     = y : merge (x:xs) ys
        | otherwise = x : merge xs ys

instance (Eq a, Ord a) => Monoid (Set a) where
  mempty = Set []



------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

-- Using typeclasses for operations
class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

data Add2 = Add2 Int Int deriving Show
data Subtract2 = Subtract2 Int Int deriving Show
data Multiply2 = Multiply2 Int Int deriving Show

instance Operation2 Add2 where
  compute2 (Add2 i j) = i + j
  show2 (Add2 i j) = show i ++ "+" ++ show j

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i - j
  show2 (Subtract2 i j) = show i ++ "-" ++ show j

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i * j
  show2 (Multiply2 i j) = show i ++ "*" ++ show j

-- Using algebraic data types directly
data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
                deriving Show

compute1 :: Operation1 -> Int
compute1 op = case op of
  Add1 i j -> i + j
  Subtract1 i j -> i - j
  Multiply1 i j -> i * j

show1 :: Operation1 -> String
show1 op = case op of
  Add1 i j -> show i ++ "+" ++ show j
  Subtract1 i j -> show i ++ "-" ++ show j
  Multiply1 i j -> show i ++ "*" ++ show j



------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String    -- contains at least one of given characters
  | DoesNotContain String  -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement  -- or'ing
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed "" _ = False
passwordAllowed psw (MinimumLength minL) = length psw >= minL
passwordAllowed psw (ContainsSome chk) = containsAtLeastOne chk psw
passwordAllowed psw (DoesNotContain chk) = not (containsAtLeastOne chk psw)
passwordAllowed psw (And req1 req2) = passwordAllowed psw req1 && passwordAllowed psw req2
passwordAllowed psw (Or req1 req2) = passwordAllowed psw req1 || passwordAllowed psw req2

containsAtLeastOne :: String -> String -> Bool
containsAtLeastOne [] _ = False
containsAtLeastOne (c:cs) psw = elem c psw || containsAtLeastOne cs psw


------------------------------------------------------------------------------
data Arithmetic = Literal Integer | Operation String Arithmetic Arithmetic deriving Show

literal :: Integer -> Arithmetic
literal x = Literal x

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation symbol x y = Operation symbol x y

evaluate :: Arithmetic -> Integer
evaluate (Literal x) = x
evaluate (Operation sym x y)
  | sym == "+"  = evaluate x + evaluate y
  | sym == "*"  = evaluate x * evaluate y

render :: Arithmetic -> String
render (Literal x) = show x
render (Operation sym x y) = "(" ++ render x ++ sym ++ render y ++ ")"
