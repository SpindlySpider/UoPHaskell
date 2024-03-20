{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []
-- 1
mult10 :: [Int] -> [Int]
mult10 = map (*10) -------------
--2
onlyLowerCase :: String -> String
onlyLowerCase =  filter isLower -------------------
--3
orAll :: [Bool] -> Bool
orAll = foldr (||) False ----------
--4
sumSqaures :: [Int] -> Int
sumSqaures = foldr (+) 0 . map (^2) -- =================================
--5
zeroToTen :: [Int] -> [Int]
-- zeroToTen xs = filter (\x -> x >=0 && x<=10) xs ---------------------
-- zeroToTen  = filter (\x -> x >=0 && x<=10)  ---------------------
zeroToTen = filter (x>=0) . filter (x<=10)
-- 6 
squareRoots :: [Float] -> [Float]
squareRoots xs = map sqrt (filter (\x -> x >= 0) xs)
-- squareRoots = map . sqrt . filter (\x -> x >= 0) 
--7 
countBetween l u = length . filter (>l) . filter (<u)   
--countBetween ::  Float -> Float -> [ Float xs] = foldr (+) 0 [1 | x <- (filter (\x -> x >= l && x <= u ) xs)]  
--8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive func xs =  all (\x -> x >=0) (map func xs) 
-- do a couple more
-- alwaysPositive func xs = filter (>=0) (map func xs)

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive func = foldr(\x xs -> func x >=0)  False

--9
productSquareRoots :: [Float] -> Float 
productSquareRoots xs =  foldr (*) 1 (squareRoots xs)
--10
removeFirst :: (a->Bool) -> [a] -> [a]
removeFirst c (x:xs)  
  | c x = xs
  | otherwise = x :  removeFirst c xs  
-- 11 
removeLast :: (a->Bool) -> [a] -> [a]
removeLast c xs =  reverse (removeFirst c (reverse xs))
--12
zeroToTen1 :: [ Int] -> [Int]
zeroToTen1 = filter (\x -> x >=0 && 10>= x)
--13
--a
alwaysPositive1 :: ( Float -> Float ) -> [ Float ] -> Bool
alwaysPositive1 func = foldr(\x xs -> func x >= 0)  False
--b
-- productSquareRoots :: [Float] -> Float 
-- productSquareRoots xs =  foldr (*) 1 (squareRoots xs)
--c
reverse1 :: [a] -> [a]
reverse1 = foldr (\x xs -> xs ++ [x] ) []
