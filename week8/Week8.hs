helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

-- Q1
greeting :: IO ()
greeting = do
    putStr "enter name: "
    str <- getLine
    putStrLn ("Hello, " ++ str)
--Q2
addTwoNumbers :: IO ()
addTwoNumbers = do
    putStr "enter 1 int: "
    number1 <- getLine
    putStr "enter 2 int: "
    number2 <- getLine
    let integer1 = read number1 :: Int
    let integer2 = read number2 :: Int
    let result = show ( integer1 + integer2 ) :: String 
    putStrLn( result  )
--Q3 
copyFile :: IO()
copyFile = do 
  putStr "Enter the filename: "
  name <- getLine
  contents <- readFile name
  putStr contents
  putStr "Enter new filename "
  newName <- getLine
  writeFile newName contents 
--Q4
buildList :: [String] -> IO ()
buildList strList = do 
    putStr "Enter a line: "
    newStr <- getLine
    if null newStr
      then putStrLn ("List is now:" ++ (show strList)) 
      else do
        putStrLn ("List is now:" ++ (show strList)) 
        buildList ([newStr] ++ strList)
listBuilder :: IO ()
listBuilder = buildList []
--Q5
sumBuilder :: IO ()
sumBuilder = do 
  putStr "Enter number to sum: "
  newInt <- getLine 
  buildSum (read newInt :: Int) []
  
buildSum :: Int -> [Int] -> IO ()
buildSum 0 list = (putStrLn . show) (foldr (+) 0 list)  
buildSum count list = do
    putStr "Enter a int: "
    newInt <- getLine
    buildSum (count-1) ([read newInt :: Int] ++ list)

--Q6
--a
addWord :: String -> [String] -> [String]
addWord word list = [word]++list
--b
wordsToString :: [String] -> String
wordsToString list = (foldr (++) "" . map (\item -> if item == last list then item else item ++ "\n")) list 
--c
wordsOfLength :: Int -> [String] -> [String]
wordsOfLength len list = filter (\x -> length x== len) list
-- user interface
--a , b , c , d
-- main :: IO ()
-- main = do
--   rawContent <- readFile "words.txt"
--   let content =  read rawContent :: [String]
--   let newlist = addWord "lemon" content
--   putStrLn (wordsToString newlist)
--   writeFile "words.txt" (show newlist :: String)
--   return ()
--
-- user interface part2 
-- a ,b ,c ,d
main :: IO ()
main = startup


startup :: IO()
startup = do
  rawContent <- readFile "words.txt"
  let content =  read rawContent :: [String]
  getUserChoice content

addWordUser :: [String] -> IO ()
addWordUser list = do
  putStr "add new word: "
  rawChoice <- getLine
  getUserChoice (addWord rawChoice list)

getUserChoice ::[String] -> IO()
getUserChoice list = do 
  displayMenu 
  rawChoice <- getLine
  let choice = read rawChoice :: Int 
  if ( choice  ==  1 )  
    then addWordUser list 
  else if (choice == 2) 
    then displayAllWords list
  else if (choice == 3) 
    then displayAllWordsOfLength list
  else
    save list

save :: [String] -> IO ()
save list = writeFile "words.txt" (show list :: String)

displayAllWords ::[String] -> IO ()
displayAllWords list = do
  putStrLn (wordsToString list) 
  getUserChoice list


displayAllWordsOfLength ::[String] -> IO ()
displayAllWordsOfLength list = do
  putStr "filter by length (enter Int): "
  rawChoice <- getLine
  let choice = read rawChoice :: Int
  putStrLn (show (wordsOfLength choice list) :: String)
  getUserChoice list

displayMenu :: IO () 
displayMenu = do 
  putStrLn "1) add words to list"
  putStrLn "2) display all words"
  putStrLn "3) display all words of given length"
  putStrLn "4) exit"
  putStr "your choice: "
