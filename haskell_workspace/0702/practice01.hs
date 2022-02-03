main = do
    a <- getLine
    putStrLn a

-- data Tree = Leaf | Node Tree Int Tree
--     deriving (Show, Eq)

-- t1 = Node Leaf 1 Leaf
-- t3 = Node Leaf 3 Leaf
-- t5 = Node Leaf 5 Leaf
-- t4 = Node t3 4 t5
-- t2 = Node t1 2 t4



-- member :: Int -> Tree -> Bool
-- member _ Leaf = False
-- member x (Node l y r) =
--     x == y || member x l || member x r

-- inorder :: Tree -> [Int]
-- inorder Leaf = []
-- inorder (Node l x r) =
--     inorder l ++ [x] ++ inorder r

-- preorder :: Tree -> [Int]
-- preorder Leaf = []
-- preorder (Node l x r) =
--     [x] ++ preorder l ++ preorder r


-- class Size a where
--     size :: a -> Int

-- instance Size [a] where
--     size xs = length xs


-- instance Size Tree where
--     size Leaf = 0
--     size (Node l _ r) = 1 + size l + size r


-- totalSize :: Size a => [a] -> Int
-- totalSize xs = sum [size x | x <- xs]


-- instace Show Tree where
--     show Leaf = "Leaf"
--     show (Node l x r) =
--         "(Node" ++ 
--         show l ++ " " ++
--         show x ++ " " ++
--         show r ++ ")"  

-- instance Eq Tree where
--     Leaf == Leaf == True
--     Node l1 x1 r1 == Node l2 x2 r2 =
--         x1 == x2 &&
--         l1 == l2 &&
--         r1 == r2
--     _ == _=False


-- 2. Eq a => 是类型约束，表示 a 类型需要是 Eq 的实例。
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup x []   = Nothing
myLookup x ((y, z) : a)
    | x == y    = Just z
    | otherwise = myLookup x a



-- >>>myLookup "1" [("1", "wang"), ("2", "zhou")]
-- Just "wang"
--


















