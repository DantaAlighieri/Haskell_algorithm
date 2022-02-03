-- unzip 将map的key和value分别取出放到2个list中
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y) : zs) = (x : xs, y : ys)
    where (xs, ys) = myUnzip zs



-- myUnzip1 :: [(a, b)] -> ([a], [b])
-- myUnzip1 [] = []
-- myUnzip1 (x, y) : ys = (x , xs) : (y, ys)
--     where 

-- >>> myUnzip [(1,4), (2,5), (3, 6)]
-- ([1,2,3],[4,5,6])
--

myZip :: [a] -> [a] -> [(a, a)]
myZip [] [] = []
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- >>>myZip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]
--


-- >>>myZip [1,2,3] []
-- []
--

myZipWith f [] [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys


-- >>> myZipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]
--


-- power 列出某一个数组的所有组合，包含空 冪集合
power :: [a] -> [[a]]
power [] = [[]]
power (x : xs) =  power xs ++ [ (x : ys) | ys <-  power xs]


-- power [] = [[]]
-- power [1] = [[] [1]]
-- power [1,2] = [[], [1], [2], [1,2]]
-- power [1,2,3] = [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]

-- >>> power [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
--


-- add :: Ord a => a -> Tree a -> Tree a
-- add x Leaf = Leaf x Leaf
-- add x (Node l y r) 
--     | x == y = Node l y r
--     | x >  y = Node l y (add x r)
--     | otherwise = Node (add x l) y r


-- 去重
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) 
    | x `elem` xs = nub xs
    | otherwise = x : nub xs

-- >>> nub [3,11,2,2,3,3]
-- [11,2,3]
--

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x : xs) 
    | x `elem` (nub1 xs) = nub1 xs
    | otherwise = x : nub1 xs

-- >>> nub1 [3,11,2,2,3,3]
-- [11,2,3]
--

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x : xs) = x : [y | y <- nub2 xs, y /= x]

-- >>> nub2 [3,1,2,2,3,3]
-- [3,1,2]
--


-- 根据key获取map中的value  lookup
monthNames = [(1, "Jan"), (2, "Feb")]


months n =
    case lookup n monthNames of
        Just s -> s
        Nothing -> "nothing"

-- >>> months 1
-- "Jan"
--



data Tree a = Leaf | Node (Tree a) a (Tree a)

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node l y r)
    | x == y    = True
    | x < y     =member x l 
    | otherwise = member x r

add :: Ord a => a -> Tree a -> Tree a
add x Leaf = Node Leaf x Leaf
add x (Node l y r)
    | x == y = Node l y r
    | x <  y = Node (add x l) y r
    | x >  y = Node l y (add x r)


depth :: Tree a -> Int
depth Leaf = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

-- >>> depth 1 2 3
-- <interactive>:6198:2-12: error:
--     ? Couldn't match expected type et0 -> t1 -> tf
--                   with actual type eIntf
--     ? The function edepthf is applied to three arguments,
--       but its type eTree a0 -> Intf has only one
--       In the expression: depth 1 2 3
--       In an equation for eitf: it = depth 1 2 3
--     ? Relevant bindings include it :: t (bound at <interactive>:6198:2)
--


-- 获取二叉搜索树中最大的元素，其实就是取最右边的元素
maxElement :: Ord a => Tree a -> a
maxElement Leaf = error "maxElement" 
maxElement (Node _ x Leaf) = x
maxElement (Node _ _ r) = maxElement r

-- 删除二叉树中的某一个元素

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node l y r)
    | x < y = Node (delete x l) y r
    | x > y = Node l y (delete x r)
delete _ (Node l _ Leaf) = l
delete _ (Node Leaf _ r) = r
delete _ (Node l _ r) = Node (delete y l) y r
    where y = maxElement l

-- >>>
