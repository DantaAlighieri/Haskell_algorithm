-- homework 1/5
-- myFoldr 给定一个函数，参数a，和数组b，数组b从后往前一次对a进行函数指定的操作
-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f e [] = e
myFoldr f e (x : xs) = f x (myFoldr f e xs)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = myFoldr (+) x xs


-- addplus1 homework 2/5
-- >>> [x | x <- x `mod` 2 == 1, x <- ]
-- addplus1 :: (a -> b -> a) -> [a] -> b -> [a]
addplus1 [] = []
addplus1 (x : xs) = [x + 1| x `mod` 2 == 1] : addplus1 xs

-- 2
oddplus1' :: [Int] -> [Int]
oddplus1' xs = 
    map (+1) (filter (\x -> mod x 2 == 1) xs)

-- 3
oddplus1'' :: [Int] -> [Int]
oddplus1'' xs = [x + 1 | x <- xs, mod x 2 == 1]


-- merge  homework 3/5
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge (x : xs) ys = x : merge xs ys

-- 有序数组合并
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] x = x
merge' x [] = x
merge' (x : xs) (y : ys) | x < y = x : (merge' xs (y : ys))
                         | otherwise = y : (merge' (x : xs) ys)

--フリーさ
merge_2 :: [Int] -> [Int] -> [Int]
merge_2 [] []                = []
merge_2 [] (y :ys)           = y : ys
merge_2 (x : xs) []          = x : xs
merge_2 (x : xs) (y : ys)    | x <= y    = x: merge xs (y : ys)
                             | otherwise = y: merge (x : xs) ys


-- split  homework 4/5
split :: [Int] -> ([Int],[Int])
split [] = ([],[])
split [x] | (x `mod` 2 == 1) = ([x],[])
          | otherwise = ([], [x])
split x = (oddlist x, evenlist x)

oddlist :: [Int] -> ([Int])
oddlist [] = []
oddlist (x : xs) | (x `mod` 2 == 1) = x : oddlist xs
                 | otherwise =  oddlist xs

evenlist :: [Int] -> ([Int])
evenlist []= []
evenlist (x : xs) | (x `mod` 2 == 0) =  x : evenlist xs
                 | otherwise = evenlist xs

-- split_1 [x_1, x_2, x_3, x_4, x_5, x_6] = ([x_1,x_3,x_5], [x_2, x_4, x_6])
split_1 :: [a] -> ([a], [a])
split_1 [] = ([], [])
split_1 (z : zs) = (z : ys, xs)
    where (xs, ys) = split_1 zs


split_2 :: [a] -> ([a], [a])
split_2 [] = ([], [])
split_2 [x] = ([x], [])
split_2 (x : y : zs) = (x : xs, y : ys)
    where (xs, ys) = split_2 zs

-- merge sort homework 4/5
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort ys) (msort zs)
          where (ys, zs) = split_1 xs
