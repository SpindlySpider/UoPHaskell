
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- Q1
-- data Month = December  | January | Febuary | March  | April | May | June | July | August | September|October | Novemeber
--   deriving (Eq,Ord,Show,Read)
-- data Season = Summer | Spring | Autumn | Winter
--   deriving (Eq,Ord,Show,Read)
data Month = December  | January | Febuary | March  | April | May | June | July | August | September|October | Novemeber
  deriving (Eq,Show,Read)
data Season = Summer | Spring | Autumn | Winter
  deriving (Eq,Show,Read)
--Q2
-- season :: Month -> Season
-- season m 
--   | m  > March && m <= May = Spring
--   | m <= August = Summer 
--   | m <= Novemeber = Autumn
--   | otherwise = Winter
------
season :: Month -> Season
season m 
  | m  `elem` [March, April,May] = Spring
  | m `elem` [June ,July, August]= Summer 
  | m `elem` [September,October,Novemeber]  = Autumn
  | otherwise = Winter

-- using length as a varaible of how many times to do 
-- Q3
numberOfday :: Month -> Int -> Int
numberOfday m y
  | y `mod` 4 == 0 && m == Febuary = 29

  | m `elem`  [January,March,May,July,August, October,December] =31
  | m `elem` [April,June,September, Novemeber] = 30
  | otherwise = 28
--Q4
data Point = Point Float Float
  deriving (Eq,Show)
--Q5 
data PositionedShape =  PositionedShape (Circle Float) Point | PositionedShape (Rectangle Float) Point
  deriving(Eq,Show)
--Q6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (Shape (Point x y ) ) dx dy = Shape (Point (x+dx)  (y+dy))
--Q7
numberOfNodes:: Tree -> Int 
numberOfNodes (Node _ lt rt) = 1 + numberOfNodes lt + numberOfNodes rt
numberOfNodes null = 0
--Q8 
isMember :: Int -> Tree -> Bool 
-- isMember v (Node nv lt rt)
--   | v /=nv =  isMember v lt || isMember v rt
--   | otherwise = True
isMember v (Node nv lt rt) = if v /=nv then isMember v lt || isMember v rt else True
isMember v null = False
--Q9
leaves :: Tree -> [Int] 
leaves (Node v Null Null) = [v]
leaves (Node v l r ) = leaves l ++ leaves r
leaves null = []
--Q 10
inOrder :: Tree -> [Int]
inOrder (Node v Null r) = [v]
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r
inOrder  null = []
--Q11
insert :: Int -> Tree -> Tree 
insert v Null = Node v Null Null
insert v (Node nv l r)
  | v < nv = Node nv (insert l) r
  | otherwise = Node nv l (insert r)
-- insert v (Node nv Null r)
--   | v < nv = (Node nv (Node v Null Null) r)
--   | otherwise = (Node nv Null (insert v r))
-- insert v (Node nv l r)
--   | v < nv = (Node nv (insert v l) r)
--   | otherwise = (Node nv l (insert v r))
-- insert _ _ = null
-- insert v (Node nv Null Null) = if v < nv then (Node nv (Node v Null Null ) Null) else (Node nv Null (Node v Null Null ))
-- insert v (Node nv Null r) = if v < nv then (Node nv (Node v Null Null) r) else (Node nv Null (insert v r))
-- insert v (Node nv l r) = if v < nv then (Node nv (insert v l) r) else (Node nv l (insert v r))
-- insert _ _ = Null
--Q12
listToSearchTree :: [Int] -> Tree
listToSearchTree (x:xs) = foldr insert (Node x Null Null) xs 
binaryTreeSort :: [Int] -> [Int]
binaryTreeSort = inOrder . listToSearchTree  
