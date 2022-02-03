data AVLTree a = Leaf | Node (AVLTree a) Int Int (AVLTree a)
    deriving Eq

    
depth Leaf = 0
depth (Node l x d r) = d

