import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
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

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)


--Q1
sumDifference:: Int -> Int -> (Int,Int)
sumDifference a b = (a+b,a-b)
--Q2
grade ::StudentMark -> Char 
grade (_,mk)
  | mk > 70 = 'A'
  | mk > 60 = 'B'
  | mk > 50 = 'C'
  | mk > 40 = 'D'
  | otherwise = 'F'
--Q3
capMark :: StudentMark -> StudentMark
capMark (name,mk)
  | mk >= 40 = (name,40)
  | otherwise = (name,mk) 
--Q4
firstNumbers :: Int -> [Int]
firstNumbers n 
  | n == 0 = []
  |otherwise = [1..n] 
--Q5
firstSquares :: Int -> [Int]
firstSquares n = [x^2|x<-[1..n]] 
-- lambda function to map to each input
-- Q6
capitalise :: String -> String
capitalise str = [toUpper c|c<-str]
--Q7 
onlyDigits :: String -> String
onlyDigits str = [c | c<-str, isNumber c ]
--Q8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmk = [capMark st | st <- stmk ]
--Q9
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmk = [(st, grade (" ",mk)) | (st, mk) <- stmk]
--Q10
-- duplicate :: String -> Int -> String 
-- duplicate str n = concat [ str | _ <- [1..n] ]
duplicate :: String -> Int -> String 
duplicate str n 
  | n == 0 = ""
  | otherwise = str ++ duplicate str (n-1) 
--Q11
divisors :: Int -> [Int]
divisors n  = [ i| i <- [1..n], n `mod` i == 0 ]
--Q12
isprime :: Int -> Bool
isprime n =  length (divisors(n)) == 2
--Q13
split:: [(a,b)] -> ([a],[b])
-- split list =  ( map fst list, map snd list) 
split list = ( [ a | (a,_) <- list ], [ b | (_,b) <- list ] )
