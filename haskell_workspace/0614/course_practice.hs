-- 递归实现计算数组长度
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 递归实现两个数组合并
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys


sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- 递归实现函数计算
myGcd :: Int -> Int -> Int
myGcd x y | y == 0 = x
          | y > x = myGcd y x
          | otherwise = myGcd (x - y) y

-- 递归 实现打印从x到y数字之间的所有的数
range :: Int -> Int -> [Int]
range x y | x < 0 || y < 0 = error "x,y cannot be negative！"
          | y < x = []
          | otherwise = x : range (x + 1) y
        
-- 向有序数组插入元素 : 链接数组 | 表示枚举
insert :: Int -> [Int] -> [Int]
insert x [] = x:[]
insert x (y : ys) | x < y = x : y : ys
                  | otherwise  = y : insert x ys


-- 递归实现插入排序
isort :: [Int] -> [Int]
isort [] = []
isort (x : xs) = insert x (isort xs)


-- lambda表达式
-- >>>(\x -> x + x) 5 10


-- map
-- 取一个函数和list作为参数，遍历该list，将每个元素带入函数进行计算，得到一个新的list
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = f x : myMap f xs

-- filter
-- 取一个函数和list作为参数，遍历该list，将每个元素带入函数进行计算，为true的情况，获取该元素，形成新的list
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x : xs) 
            | p x = x : myFilter p xs
            | otherwise = myFilter p xs


-- partition 将数组按照条件分为A数组和B数组
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x : xs)
            | p x = (x : ys, zs)
            | otherwise = (ys, x : zs) 
            where (ys, zs) = partition p xs


-- myFoldl 给定一个函数，参数a，和数组b，a按照函数规则分别对b进行处理，得到a
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e [] = e
myFoldl f e (x : xs) = myFoldl f (f e x) xs


-- myFoldr 给定一个函数，参数a，和数组b，数组b从后往前一次对a进行函数指定的操作
-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f e [] = e
myFoldr f e (x : xs) = f x (myFoldr f e xs)



-- list cmprehension
-- >>> [x + y | x <- [10, 20], y <- [1, 2]]
-- [11,12,21,22]
--


-- quick sort
qsort [] = []
qsort (x : xs) = qsort [ y | y <- xs, y < x ] ++ [x] ++ qsort [ y | y <- xs, y >= x]


-- merge sort
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x : xs) ys = x : merge xs ys

msort [] = []
msort (x : xs) ys = merge(msort ys) (msort zs)


