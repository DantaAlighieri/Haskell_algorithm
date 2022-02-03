-- prefix 可以看作有一个窗口，这个窗口可以容纳该list个元素，窗口向前移动
prefix :: [a] -> [[a]]
prefix [] = [[]]
prefix (x : xs) = [] : [(x : y) | y <- prefix xs]
-- 解题思路
-- prefix (3 : 2,1) = prefix [] : [(3 : [], [2], [2,1])]
-- prefix [3]     = [[],                         [3]]
-- prefix [3,2]   = [[],      [3],               [3,2]]
-- prefix [3,2,1] = [[],      [3],        [3,2], [3,2,1]]
-- >>>prefix [3,2,1]
-- [[],[3],[3,2],[3,2,1]]
--




-- suffix 窗口向后移动，将窗口中的元素打印出来
suffix [] = [[]]
suffix (x :xs) = (x : xs) : suffix xs

-- 解题思路
-- suffix (3 : 2,1) = (3 : 2,1) ++ suffix [2,1]
-- suffix [3]     = [[3],                 []]
-- suffix [3,2]   = [[3,2],          [2], []]
-- suffix [3,2,1] = [[3,2,1], [2,1], [1], []]
-- >>> suffix [3,2,1]
-- [[3,2,1],[2,1],[1],[]]
--


-- interleave 将元素插入数组中的所有插入方法
-- 解题思路
-- >>> zip (prefix [2,3]) (suffix [2,3])
-- [([],[2,3]),([2],[3]),([2,3],[])]
--
-- >>>[xs ++ [1]++ ys | (xs, ys) <- zip (prefix [2,3]) (suffix [2,3])]
-- [[1,2,3],[2,1,3],[2,3,1]]
--
interleave x xs = [ys ++ [x]++ zs | (ys, zs) <- zip (prefix xs) (suffix xs)]

-- permutations 将数组中的所有元素进行排列组合
-- 解题思路，将数组中每个元素挨个遍历以后调用interleave方法对剩余元素进行组合
permutations [] = [[]]
permutations (x : xs) = [zs | ys <- permutations xs, zs <- interleave x ys]

-- >>> permutations [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
--

safe :: Int -> Int -> Int -> Int -> Bool
safe i j i' j' =
    (
        i /= i' &&
        j /= j' &&
        i - i' /= j - j' &&
        i - i' /= j' - j ) ||
     (  i == i' && j == j')

-- >>> safe 1 3 2 4
-- False
--

ok xs =
    all
        (\i -> all (\j -> safe i (xs !! i) j (xs !! j)) [0 .. n-1])
        [0 .. n-1]
    where n = length xs

nqueen :: Int -> [[Int]]
nqueen n = [ xs | xs <- permutations [0 .. n-1], ok xs]

-- >>>nqueen 4
-- [[2,0,3,1],[1,3,0,2]]
--


ok' :: [Int] -> Bool
ok' [x1,x2,x3,
    x4,x5,x6,
    x7,x8,x9] =
    x1 + x2 + x3 == 15 &&
    x4 + x5 + x6 == 15 &&
    x7 + x8+ x9 == 15 &&    
    x1 + x4 + x7 == 15 &&
    x2 + x5 + x8 == 15 &&
    x3 + x6 + x9 == 15 &&
    x1 + x5 + x9 == 15 &&
    x3 + x5 + x7 == 15 

magicsquare :: [[Int]]
magicsquare = [xs | xs <- permutations [1 .. 9], ok' xs]

-- >>>magicsquare
-- [[6,1,8,7,5,3,2,9,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,9,2,3,5,7,8,1,6]]
--



comb :: [a] -> Int -> [[a]]
comb _ 0  = [[]]
comb [] _ = []
comb (x : xs) n = [x : y | y <- comb xs (n-1)] ++ comb xs n

-- >>> comb [10,20,30,40] 5
-- []
--
