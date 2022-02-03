data Tree = Leaf | Node Tree Int Tree

t1 = Node Leaf 1 Leaf
t3 = Node Leaf 3 Leaf
t5 = Node Leaf 5 Leaf
t4 = Node t3 4 t5
t2 = Node t1 2 t4


-- postorder
postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) =
    postorder l ++ postorder r ++ [x]

--去重
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs)
    | elem x (nub xs) = nub xs
    | otherwise = x : nub xs


nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x : xs)
    | elem x ys = ys
    | otherwise = x : ys
    where ys = nub xs

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x : xs) = x : [y | y <- nub2 xs, x /= y]


nub3 :: Eq a => [a] -> [a]
nub3 [] = []
nub3 (x : xs) = x : [y | y <- nub2 xs, x /= y]


nub4 :: Eq a => [a] -> [a]
nub4 [] = []
nub4 (x : xs) = x : nub4 [x' | x' <- xs, x /= x']


MonthName1 n =
    case lookup n monthNames of
        Just s -> s
        Nothing -> "Not found"

MonthName2 n 




























