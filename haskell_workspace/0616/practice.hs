-- sumList :: [Int] -> Int
-- sumList [10,20,30] = 60
-- sumList [20,30] = 50
-- sumList [30] = 30
-- sumList [] = 0
-- sumList (x : xs) = ..

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs



myGcd :: Int -> Int -> Int
myGcd x y | y < 0    = error "myGcd"
          | y > x     = myGcd y x
          | otherwise = myGcd (x - y) y



-- range :: m n = [m, m + 1, m + 2, ... , n]
range :: Int -> Int -> [Int]
range m n | m > n = []
          | otherwise = m : range (m + 1) n

-- homework 2/3 
-- range :: m n => | x
-- [X | X <- [m..n], X > m and X < n]


-- evens :: [a] -> [a]
-- evens [] = []
-- evens (x : xs) =  [x | x `mod` 2 == 0] : evens xs



insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y
                  then x:y:ys
                  else y : insert x ys


isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x : xs) = insert x (isort xs)

-- isort (5 : 2 : 3 : 2 : [])
-- = insert x (insert 2 (insert 3 (insert 2 (insert (1)))))

