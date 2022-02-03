-- >>> zip [1,2,3] ["a", "b", "c"]
-- [(1,"a"),(2,"b"),(3,"c")]
--

-- myZip [] [] = ..
-- myZip (x: xs) (y : ys) = (x, y) : myZip xs ys

-- myZipWith f [] [] = []
-- myZipWith f (x: xs) (y : ys) = f x y : myZipWith xs ys


suffixes []      = [[]]
suffixes (x :xs) = (x :xs) : suffixes xs

-- suffixes [2, 3] = [[2, 3], [3], []] = [2,3]
-- suffixes [3] = [[3], []]
-- suffixes [] = [[]]

-- prefixes [2,3] = [[], [2], [2, 3]]
-- prefixes [3] = [[], [3]]
-- prefixes [] = [[]]

prefixes [] = [[]]
prefixes (x : xs) = [] : [x : ys | ys <- prefixes xs]



-- >>> [ys ++ [1] ++ zs | (ys, zs) <- zip (prefixes [2,3], suffixes [2,3])]
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\0630\N-Queens.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\0630\N-Queens.hs:42:1-10: error:
--     Parse error: module header, import declaration
--     or top-level declaration expected.
-- Failed, no modules loaded.
--


interleave x xs = 
    [ys ++ [x] ++ zs | (ys, zs) <- zip (prefixes xs) (suffixes xs)]

-- >>> interleave 1 [2,3]



permutations [] = [ [] ]
permutations (x : xs) = [zs | ys <- permutations xs, zs <- interleave x ys ]

-- >>> permutations [1, 2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
--


myZipWith1 f [] [] = []

Lemma :
revapp xs ys = rev xs ++ ys
    for all lists xs and ys.

Proof.
    (まだ説明していない)
    （次回の宿題）

Theorem.

rev xs = revapp []
    for all lists xs.
Proof.
rhs = revapp xs []
    = rev xs ++ [] by Lemma
    = rev xs       by Ex. 1
    = lhs
QED