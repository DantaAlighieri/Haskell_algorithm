-- 無限リスト 7-12

-- from :: Int -> [Int]
from n = n : from (n + 1)

-- myTake :: Int -> [Int] -> [Int]
myTake 0 _ = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- >>> myTake 20 (from 12)
-- [12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
--

numbers1 = 1 : map (+1) numbers1
numbers2 = 1 : [x+1 | x  take 10 numbers1
[1,2,3,4,5,6,7,8,9,10]
*Main> take 10 numbers2
[1,2,3,4,5,6,7,8,9,10]
*Main> take 10 numbers3
[1,2,3,4,5,6,7,8,9,10]



