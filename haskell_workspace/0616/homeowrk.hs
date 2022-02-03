sumList :: (Num b) => [a] -> b
sumList [] = 0
sumList (x : xs) = x + sumList xs





oddplus1 :: (Num a) => [a] -> [a]
oddplus1 [] = []
oddplus1 (x : xs) = map(+1) (filter (\x -> mod x 2 == 1) xs)
