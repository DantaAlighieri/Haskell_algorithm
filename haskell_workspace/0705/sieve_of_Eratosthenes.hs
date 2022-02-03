
                                       1  2  1  2  1  2  1  2
filterout 2 [3,4,5,6,7,8,9,10]      = [3, -, 5, -, 7, -, 9, -]
                                       1  2  3  1  2  3
filterout 3 [-, 5, -, 7, -, 9]      = [-, 5, -, 7, -, -]

sieve :: [Int] -> [Int]
sieve (2 : xs) = 2 : filterout 2 xs


primes :: [Int]
primes = sieve [2..]
