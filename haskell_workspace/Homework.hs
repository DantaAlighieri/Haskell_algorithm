-- homework 1/3
myGcd :: Int -> Int -> Int
myGcd x y | y == 0    = x
          | y > x     = x * x + y * y
          | otherwise = (x - y) * (x - y) + y * y

-- homework 2/3
range :: m n = [m, m + 1, m + 2,... n]

range x y | x > y = []
          | otherwise = range x y
-- homework 2/3          
-- [X | X <- [m..n], X > m and X < n]


insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y
                  then x:y:ys
                  else y : insert x ys

-- homework 3/3
isort [] = []
isort [x] = [x]
isort (z : xs) = insert x isort xs