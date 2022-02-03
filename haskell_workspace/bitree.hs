
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Eq

instance Show a => Show (Tree a) where
  show Leaf         = "."
  show (Node l x r) = "(" ++ show l ++ " " ++ show x ++ " " ++ show r ++ ")"


member :: Ord a => a -> Tree a -> Bool
member x Leaf = False
member x (Node l y r)
  | x == y    = True
  | x < y     = member x l
  | otherwise = member x r

add :: Ord a => a -> Tree a -> Tree a
add x Leaf    = Node Leaf x Leaf
add x t@(Node l y r)
  | x == y    = t
  | x < y     = Node (add x l) y r
  | otherwise = Node l y (add x r)