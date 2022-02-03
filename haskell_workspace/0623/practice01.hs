main = do
    a <- getLine
    putStrLn a





-- nub :: Eq  a => [a] -> [a]
-- nub [] = []
-- nub (x : xs) 
--     | elem x (nub xs) = nub xs
--     | otherwise = x : nub xs


-- >>> nub [3,3,2,9,4,5,1,1,2,3,3]
-- [9,4,5,1,2,3]
--

nub :: Eq  a => [a] -> [a]
nub [] = []
nub (x : xs) 
    | elem x ys = ys
    | otherwise = x : ys
    where ys = nub xs

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x : xs) = x : [y | y <- nub2 xs, y /= x]


-- >>> nub2 [2,3,4,2,1]
-- [2,3,4,1]
--


monthNames = [("1", "Jan"), ("2", "Feb")]

monthName n = 
    case lookup n monthNames of
        Just s -> s
        Nothing -> "Not found"

-- >>>monthName "1"
-- "Jan"
--
