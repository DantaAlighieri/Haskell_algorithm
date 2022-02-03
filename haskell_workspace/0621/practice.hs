data Treee = Leaf | Node Tree Int Tree
    deriving Show

data Treee = Leaf | Node Tree Int Tree
    deriving Eq

data Treee = Leaf | Node Tree Int Tree
    deriving (Show, Eq)

t1 = Node Leaf 1 Leaf 
t3 = Node Leaf 3 Leaf
t5 = Node Leaf 5 Leaf
t4 = Node t3 4 t5
t2 = Node t1 2 t4

member :: Int -> Int -> Bool
member x Leaf = false
member x (Node 1 y r) 
    | x == y = True
    | member x 1 = True
    | member x r = True
    |otherwise = False

-- >>> member 6 t2

member_2 :: Int -> Int -> Bool
member_2 x Leaf = false
member_2 x (Node 1 y r) =
    x == y || member_2 x l || member_2 xr


-- Tree Traversal

--in-order

inorder :: Tree -> [Int]
inorder Leaf = []
inorder (Node 1 x r) =
    inorder 1 ++ [x] ++ inorder r

--postorder
postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node 1 x r) =
    postorder 1 ++ postorder r ++ [x]


-- preorder
preorder :: Tree -> [Int]
preorder Leaf = []
preorder (Node 1 x r) =
    [x] ++ preorder 1 ++ preorder r


-- Type Classes for Function Overloading
class Size a where
    size :: a -> Int

instance Size [a] where
    size xs = length xs

instance Size Tree where
    size Leaf = 0
    size (Node l x r) = l + size l + size r

totalSize :: size a => [a] -> Int
totalSize xs = sum [ size x | x <- xs]

-- >>> totalSize [t1, t4]

instance Show Tree where
    show Leaf = "Leaf"
    show (Node 1 x r) =
        "(Node " ++
        show l ++ " " ++ 
        show x ++ " " ++
        show r ++ ")"


-- Equality on Tree
-- >>> t1 == t4

instance Eq Tree where
    Leaf == Leaf == True
    Node l1 x1 r1 == Node l2 x2 r2 =
        x1 == x2 &&
        l1 == l2 &&
        r1 == r2
    _ == _ == False
-- >>> t4 == t2

data Tree = Leaf | Node Tree Int Tree
type Forest = [Tree]

-- data [a] = [] / a : [a]
-- data Bool = False / True
-- data Maybe a = Nothing / Just a
-- data String = [Char]

-- >>> ['a', 'b', 'c']
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\0621\practice.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\0621\practice.hs:85:5: error:
--     parse error (possibly incorrect indentation or mismatched brackets)
-- Failed, no modules loaded.
--


looup 2 [(1, "jan"), (2, "feb")]
-- just "feb"

myLookup :: Eq a => a -> [(a, b)] -> maybe b
myLookup _ [] = Nothing
myLookup x ((y, z) : a)
    | x == y = Just z
    | otherwise = myLookup x a 
