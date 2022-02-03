data Tree = Leaf | Node Tree Int Tree

postorder :: Tree ->[Int]
postorder Leaf = []
postorder (Node l x r) =
    postorder l ++ postorder r ++ [x]

-- t1 = Node Leaf 1 Leaf
-- t3 = Node Leaf 3 Leaf
-- t5 = Node Leaf 5 Leaf
-- t4 = Node t3 1 t5
-- t2 = Node t1 1 t4


-- nub :: Eq a => [a] -> [a]
-- nub [1,2,3,3,3,2,4,1] = [1,2,3,4]
-- nub   [2,3,3,3,2,4,1] = [2,3,4,1]
-- nub     [3,3,3,2,4,1] =   [3,2,4,1]
-- nub       [3,3,2,4,1] =   [3,2,4,1]
-- nub         [3,2,4,1] =   [3,2,4,1]
-- nub           [2,4,1] =     [2,4,1]
-- nub             [4,1] =       [4,1]
-- nub               [1] =         [1]

-- nub :: Eq a => [a] -> [a]
-- nub [] = []
-- nub (x :xs)
-- nub [3,3,2,4,1]
--  | elem 3 [3,2,4,1] = nub [3,2,4,1]
--  |elem x ??? = nub xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x :xs)
 | elem x (nub xs) = nub xs
 | otherwise = x : nub xs

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x :xs)
 | elem x ys = ys
 | otherwise = x : ys
 where ys = nub2 xs

 
nub3 :: Eq a => [a] -> [a]
nub3 [] = []
nub3 (x :xs) = x : [y | y <- nub3 xs, x /= y]

nub4 :: Eq a => [a] -> [a]
nub4 [] = []
nub4 (x :xs) = x : nub4 [x' | x' <- xs, x /= x']
 

--  monthNames = [(1, "Jan"), (2, "Feb")]
--  monthName1 n = 
--      case lookup n monthNames of
--          Just s -> s
--          Nothing -> "not found"

-- monthName2 n
--     | Just s <- lookup n monthNames = s
--     | otherwise                     = "not found"

-- absolute n 
--     | True <- n >= 0 = n
--     | Trhe <- True = -n

-- main :: IO ()
-- main = putStrln "Hello"

-- main :: IO ()
-- main = do
--     putStrln "Hello"
--     putStrln "Hello, world"

-- main :: IO ()
-- main = do
--     putStrln "Hello"
--     let x = 10 :: Int
--     putStrln("Hello, world" ++ show x)


-- main :: IO ()
-- main = do {putStrln "Hello"; let x = 10 :: Int;putStrln("Hello, world" ++ show x)}

-- myProduct [x1, ... , xn] = x1 * ... * xn
myProduct [] = 0
myProduct (x : xs) = x * myProduct xs

