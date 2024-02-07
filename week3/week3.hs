import Data.ByteString (find)
import Prelude hiding (gcd, (&&), (||))

infixr 3 &&

infixr 2 ||

-- Q1
-- A naive re-implementation of the Prelude operator ||, &&
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- False || True = True
-- True || False = True
-- False || False = False

-- (&&) :: Bool -> Bool -> Bool-- False && False = False
-- False && True = False
-- True && False = False
-- True && True = True

-- An alternative re-implementation
-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

-- Another alternative re-implementation
(||) :: Bool -> Bool -> Bool
True || _ = True
False || a = a

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && a = a

-- Q2
exOr :: Bool -> Bool -> Bool
exOr True b = not b
exOr a True = not a
exOr a b = False

-- Q3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse _ _ b = b

-- Q4
daysInMonth :: Int -> Int
daysInMonth m
  | m == 2 = 28
  | m < 8 && m `mod` 2 == 0 = 30
  | m < 8 = 31
  | m `mod` 2 == 0 = 31
  | otherwise = 30

vaildDate :: Int -> Int -> Bool
vaildDate d m = daysInMonth m >= d

-- Q5
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- Q6
sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

-- Q7
power :: Int -> Int -> Int
power _ 0 = 1
power n p = n * power n (p - 1)

-- Q8
sumFromTo :: Int -> Int -> Int
sumFromTo a b
  | a > b = 0
  | a == b = b
  | otherwise = a + sumFromTo (a + 1) b

-- Q9
absolute :: Int -> Int
absolute number = if number < 0 then 0 - number else number

-- from first worksheet
gcd :: Int -> Int -> Int
gcd a b
  | a == b = a
  | a > b = gcd diff b
  | otherwise = gcd diff a
  where
    diff = absolute (a - b)

-- Q10
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
  | n >= power s 2 = s
  | otherwise = findRoot n (s - 1)
