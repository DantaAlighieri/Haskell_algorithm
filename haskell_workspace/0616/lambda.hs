twie :: (t -> t) -> t -> t
twie f x = f (f x)




myMap :: (a -> b) -> [a] -> [b]
myMap _[] = []
myMap f (x : xs) = f x : myMap f xs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x : xs)
        | p x       = x : myFilter p xs  
        | otherwise = myFilter p xs


-- Partition

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([],[])
partition p (x : xs)
        | p x  = (x : ys, zs) 
        | otherwise = (ys, x : zs) 
        where (ys, zs) = partition p xs

-- fold functions
-- foldl foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl _ e []       = e
myFold1 f e (x : xs) = myFold1 f (f e x) xs 


