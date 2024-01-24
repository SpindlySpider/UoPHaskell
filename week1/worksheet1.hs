timesTen :: Int->Int
timesTen x=x*10

sumThree :: Int->Int->Int->Int
sumThree a b c = a+b+c

areaOfCircle :: Float -> Float 
areaOfCircle r = pi*r^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle(r) * h

distance :: Float -> Float -> Float -> Float -> Float 
distance x1 y1 x2 y2 = sqrt( (y1-y2)^2 + (x1-x2)^2 )
-- 6 

threeDifferent :: Int->Int->Int->Bool
threeDifferent a b c = a /= b &&a/=c && b /= c 

divisiableBy :: Int -> Int -> Bool
divisiableBy int1 int2 = int1 `mod` int2 == 0

isEven :: Int -> Bool 
isEven num = divisiableBy num 2

averageThree :: Int -> Int -> Int -> Float
averageThree int1 int2 int3 = fromIntegral(int1 + int2 + int3)/3


