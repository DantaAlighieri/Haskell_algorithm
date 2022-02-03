squareSum :: Int -> Int -> Int
squareSum x y = x * x + y * y

identity :: a -> a
identity x = x

sum1 :: Int -> Int
sum1 n =
    if n == 0 then 0 else n + sum1 (n -1)

sum2 :: Int -> Int
sum2 n | n == 0 = 0
       | otherwise = n + sum2 (n -1)

sum3 :: Int -> Int
sum3 0 = 0
sum3 n = sum3 (n - 1)  + n

-- n! = n * (n-1)!
-- 5! = 5 * 4 * 3 * 2 * 1 * 1 
--    = 5 * 4! 
-- 4! =     4 * 3 * 2 * 1 * 1 = 
-- 3! =         3 * 2 * 1 * 1
-- 2! =             2 * 1 * 1
-- 1! =                 1 * 1
-- 0! =                     1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


-- fib
-- 0 1  1    2   3   5
--     0+1 1+1  1+2  2+3
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
-- fib 2 = 1
-- fib 2 = 1 = 0 + 1 = fib 0 + fib 1
fib n = fib (n-2) + fib (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs
myLength (     "b" : "a" : []) = 2
myLength("c" : "b" : "a" : []) = 3 = 1 + myLength ("b" : "a" : [])
myLength(x : xs)               =     1 + myLength xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

f :: [Int] -> Int
f [] = -1
f (1 : []) = 100
f (2 : []) = 200
f (x : []) = x
f(x : (y : zs)) = x + y

-- >>> f (1 : (2 :3 []))
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\B.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\B.hs:47:36: error: parse error on input e=f
-- Failed, no modules loaded.
--

--- >>> append [1,2,3] [4,5]
--- (Error while loading modules for evaluation)
--- [1 of 1] Compiling Main             ( C:\haskell_workspace\B.hs, interpreted )
--- <BLANKLINE>
--- C:\haskell_workspace\B.hs:47:36: error: parse error on input e=f
--- Failed, no modules loaded.
---




-- >>> [1,2,3]
-- [1,2,3]
-- >>> ["a", "b", "c"]
-- ["a","b","c"]
--

-- >>> fib 10
-- <interactive>:5136:2-4: error:
--     Variable not in scope: fib :: t0 -> t
--



-- >>> factorial 10
-- 3628800
--


-- >>> sum3 100

-- <interactive>:2692:2-5: error:
--     ? Variable not in scope: sum3 :: t0 -> t
--     ? Perhaps you meant esumf (imported from Prelude)
--

-- >>> sum1 10
-- >>> squareSum 10 20
-- <interactive>:1638:2-5: error:
--     ? Variable not in scope: sum1 :: t0 -> t
--     ? Perhaps you meant esumf (imported from Prelude)
-- <BLANKLINE>
-- <interactive>:1639:2-10: error:
--     Variable not in scope: squareSum :: t0 -> t1 -> t
--
