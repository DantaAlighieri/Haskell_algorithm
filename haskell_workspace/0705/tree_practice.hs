data Tree a = Leaf | Node (Tree a) a (Tree a)

data AVLTree a = Leaf | Node (AVLTree a) a Int (AVLTree a)
depth :: AVLTree a -> Int
depth Leaf = 0
depth (Node _ _ d _) = d


node :: AVLTree a -> a -> AVLTree a > AVLTree a
node l x r = Node l x d r
    where d = max (depth l) (depth r) + 1

slope :: AVLTree a -> Int
slope Leaf = 0
slope (Node l _ _ r) = depth l - depth r

rebalance :: AVLTree a -> AVLTree a
rebalance t
    | slope t == -2 = shiftl t
    | slope t == 2  = shiftr t
    | otherwise     = t

add :: Ord a => a -> Tree a -> Tree a
add x Leaf = Node Leaf x Leaf
add x t@(Node l y r)
    | x == y        = t
    | x < y         = rebalance (node (add x l ) y r)
    | otherwise     = rebalance (node l y (add x r))

delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf                  = Leaf
delete x (Node l y r)
    | x < y = rebalance (Node (delete x l) y r)
    | x > y = rebalance (Node l y (delte x r))
delete x (Node Leaf y r) = r
-- 删除的正好是y节点，即 x等于y的时候，需要从左侧树种找出最大的元素
delete x (Node l y r) = rebalance (node (delete z l ) z r)
    where z = maxElement l


countNodes :: Tree a -> Int
countNodes (Leaf a) = 0
countNodes (Node left right) = 1 + countNodes left + countNodes right

perfect Leaf = True
perfect (Node l x r)
  | perfect l && perfect r = count l == count r
  | otherwise = False