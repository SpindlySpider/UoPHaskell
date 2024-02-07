-- Q1
absolute :: Int -> Int
absolute number
  | number < 0 = 0 - number
  | otherwise = number

-- Q2
sign :: Int -> Int
sign number
  | number < 0 = -1
  | number == 0 = 0
  | otherwise = 1

-- Q3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && a == c || b == c && c == a = 3
  | a == b || a == c || b == c = 2
  | otherwise = 0

-- Q4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = sqrt (2 * a ^ 2) + sqrt (2 * b ^ 2) + sqrt (2 * c ^ 2)

-- Q5
taxiFare :: Int -> Float
taxiFare dist
  | dist >= 10 = 7.20 + (fromIntegral (dist - 10) * 30) / 100
  | otherwise = 2.20 + fromIntegral (dist * 50) / 100

-- Q6
averageThree :: Int -> Int -> Int -> Float
averageThree int1 int2 int3 = fromIntegral (int1 + int2 + int3) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | average < a && average < b || average < a && average < c || average < b && average < c = 2
  | average < a || average < b || average < c = 1
  | otherwise = 0
  where
    average = averageThree x y z
    -- turns into float type
    a = fromIntegral x :: Float
    b = fromIntegral y :: Float
    c = fromIntegral z :: Float

-- Q7
vaildDate :: Int -> Int -> Bool
vaildDate d m
  | m < 1 || m > 12 = False
  | m == 2 && d <= 28 && d > 0 = True
  | m `elem` [1, 3, 5, 7, 8, 10, 12] && d <= 31 && d > 0 = True
  | m `elem` [4, 6, 9, 11] && d <= 30 && d > 0 = True
  | otherwise = False

-- Q8
daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | m `elem` [4, 6, 9, 11] = 30
  | m == 2 && leap == 0 = 29
  | otherwise = 28
  where
    leap = y `mod` 4
