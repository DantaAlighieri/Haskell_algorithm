a :: [[Int]]
a = [[1,2,3],[4,5,6]]


-- f2 :: Int -> Int
-- f2 c = c + 1

-- f1 :: [Int] -> [Int]
-- f1 = map f2

-- >>> map f1 a
-- [[2,3,4],[5,6,7]]
--
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)...]
d :: [(Int, Int)]
d = [(x, y) | x <- [0 ..2], y <- [0 ..2]]


-- >>> d
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
--

f3 :: [[Int]] -> [[Int]]
f3 e = map (map (+1)) e

-- >>> f3 [[]]
-- [[]]
--

-- 高阶函数
add :: Int -> Int -> Int
add a b = a + b

-- foldl
-- (add 3) 

-- >>> foldl []




coin :: Int -> [Int] -> [Int]
coin money [] = []
coin money (c :cs)
    | money - c >= 0 =
        maxLength (c:[] ++ coin (money - c) cs) (coin money cs)
    | otherwise      =
        coin money cs

maxLength :: [Int] -> [Int] -> [Int]
maxLength a b 
    | length a > length b = a
    | otherwise = b

-- >>> coin 12 [1,2,3,5,6,7,8]
-- [1,2,3,6]
--



