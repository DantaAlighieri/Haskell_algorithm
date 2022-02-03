removeNonUppercase :: [Char] -> [Char]   
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer   
factorial n = product [1..n]

circumference :: Float -> Float   
circumference r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKKY NUMBER SEVEN"
lucky x = "Sorry, you are out of luck, pal!"

factorial1 :: (Integral a) => a -> a
factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)

head' :: [a] -> a
head' [] = error "can't call head on an empty list, dummy!"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital [] = "empty string"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- 麦克西米不可思议

-- 快速排序

-- quicksort :: (Ord a) => [a] -> [a]   
-- quicksort [] = []   
-- quicksort (x:xs) =   
--   let smallerSorted = quicksort [a | a  xs, a  x]  
--        biggerSorted = quicksort [a | a  xs, a > x]   
--   in smallerSorted ++ [x] ++ biggerSorted
  
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])



map(+3) [1,5,3,1,6]
