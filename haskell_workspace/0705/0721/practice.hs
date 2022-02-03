merge :: [Int] -> [Int] -> [Int]
merge (x :xs) (y : ys)
    | x == y = x : merge xs ys
    | x < y  = x : merge xs (y : ys)
    | x > y  = y : merge (x :xs) ys
merge _ _    = error "merge"

hs :: [Int]
hs = 1 : merge (merge [2 * n | n <- hs] [3 * n | n <- hs]) [5 * n | n <- hs]


-- >>> take 20 hs
-- [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]


hs' :: [Int]
hs' = 1 : merge [1 * n | n <- hs'] [1 * n | n <- hs']


-- >>> take 20 hs'
-- [1,2,3,4,6,8,9,12,16,18,24,27,32,36,48,54,64,72,81,96]
--

-- sequence :: [Int]
-- sequence = [1, 2 ..]

-- >>>take 10 sequence
-- <interactive>:2569:9-16: error:
--     Ambiguous occurrence esequencef
--     It could refer to
--        either ePrelude.sequencef,
--               imported from ePreludef at C:\haskell_workspace\0705\0721\practice.hs:1:1
--               (and originally defined in eData.Traversablef)
--            or eMain.sequencef,
--               defined at C:\haskell_workspace\0705\0721\practice.hs:16:1
--


flatten :: [[a]] -> [a] 
flatten [[]] = [] 
flatten [(x:xs)] = flatten [xs] ++ [x] 

edges = concat [concat [[(m,n),(n,m)] | let m = t, n <- take m [0..]] ++ [(t,t)] 
      | t <- [0..]]

-- >>> take 10 edges
-- [(0,0),(1,0),(0,1),(1,1),(2,0),(0,2),(2,1),(1,2),(2,2),(3,0)]
--


-- array :: [Int] -> [(Int)]
-- array [0, 1] = [[(m,n),(n,m)] | let m = t, n <- take m [0..] ++ [(t,t)]
--  | t <- [0..]

-- power :: (Eq t1, Num t1) => (t2 -> t2) -> t1 -> t2 -> t2
-- power f 0 x = x
-- power f n x = f (power f (n - 1) x)

array1 :: [Int] -> [[Int]]
array1 [] = [[]]
-- array1 (x : xs) = [] ++ array1_sub (x : xs) [[]]
-- array1 (x : xs) = ys ++ array1_sub (x :xs) ([[x] ++ y | y <- ys] ++ array1_sub xs ys)

array1_sub :: [Int] -> [[Int]] -> [[Int]]
array1_sub [] _ = []
array1_sub (x : xs) ys = ys ++ array1_sub (x :xs) ([[x] ++ y | y <- ys] ++ array1_sub xs ys)


-- >>> array1_sub [1,2] [[]]
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\0705\0721\practice.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\0705\0721\practice.hs:65:86: error:
--     parse error on input e]f
-- Failed, no modules loaded.
--



-- >>> array1 [1,2] [[]]
-- [[1],[2],[]]
--
