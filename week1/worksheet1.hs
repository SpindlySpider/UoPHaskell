-- 1 
timesTen :: Int->Int
timesTen x=x*10
-- 2
sumThree :: Int->Int->Int->Int
sumThree a b c = a+b+c
-- 3
areaOfCircle :: Float -> Float 
areaOfCircle r = pi*r^2
-- 4 
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle(r) * h
-- 5
distance :: Float -> Float -> Float -> Float -> Float 
distance x1 y1 x2 y2 = sqrt( (y1-y2)^2 + (x1-x2)^2 )
-- 6 
threeDifferent :: Int->Int->Int->Bool
threeDifferent a b c = a /= b &&a/=c && b /= c 
-- 7 
divisiableBy :: Int -> Int -> Bool
divisiableBy int1 int2 = int1 `mod` int2 == 0
--8 
isEven :: Int -> Bool 
isEven num = divisiableBy num 2
-- 9 
averageThree :: Int -> Int -> Int -> Float
averageThree int1 int2 int3 = fromIntegral(int1 + int2 + int3)/3
-- 10
absolute :: Int -> Int 
absolute number = if number < 0 then  0 - number else number
