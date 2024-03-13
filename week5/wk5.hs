{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head :: [p] -> p
head (x : _) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "tail: empty list"

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []


-- For question 10
type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]
--1
headPlusOne::[Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) =  x+1 
--2
duplicateHead::[a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs
--3 
rotate::[a]->[a]
rotate [a] = [a]
rotate (a:b:cs) = b:a:cs
--4
listLength::[a] -> Int
listLength [] = 0 
listLength (x:xs) = 1 + listLength xs
--5 
multAll::[Int]->Int
multAll [] = 1 
multAll [a] = a 
multAll (x:xs) = x * multAll xs
--6
andAll::[Bool]->Bool
andAll [] = True
andAll [a] = a
andAll (x:xs) = x && andAll xs
--7
orAll::[Bool] -> Bool
orAll [a] = a 
orAll (x:xs) = x || orAll xs
--8
countIntegers:: Int -> [Int] -> Int 
countIntegers _ [] = 0
countIntegers n (x:xs)
  | x == n = 1 + countIntegers n xs
  | otherwise = countIntegers n xs
--9
removeAll:: Int -> [Int] -> [Int]
removeAll n arr = [x| x <- arr , x /= n  ]
--10
removeAllButFirst::Int -> [Int] -> [Int]
removeAllButFirst n (x:xs)
  | n == x = x : removeAll n xs
  | otherwise = x: removeAllButFirst n xs
  --11 
listMarks:: String -> [StudentMark] -> [Int]
listMarks _ [(st,mk)]  = []
listMarks s ((st,mk) : stmks)
  | s == st = mk : listMarks s stmks
  | otherwise = listMarks s stmks 
-- 12
sorted:: [Int] -> Bool
sorted [a,b] =  a <= b
sorted ( a : b : xs )
  | a<= b = sorted xs
  | otherwise = False
-- 13
prefix :: [Int] -> [Int] -> Bool
prefix [a] (y:ys) =  a == y  
prefix (x:xs) (y:ys)
  | x == y = prefix xs ys
  | otherwise = False 
--14
subSequence::[Int] -> [Int] -> Bool 
subSequence _ [] = False
subSequence [] _ = True
subSequence (x:xs) (y:ys)
  | prefix [x] (y:ys) = subSequence xs ys   
  | otherwise =  subSequence [x] ys 
