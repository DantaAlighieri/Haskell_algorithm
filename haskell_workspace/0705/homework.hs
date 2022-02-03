myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y) : zs) = (x : xs, y : ys) 
    where (xs, ys) = unzip zs

-- >>> myUnzip [(1, 10), (2, 20), (3, 30)]
-- ([1,2,3],[10,20,30])
--


-- P({1, 2, 3}) = {}

power [] = [[]]
power (x :xs) = power xs ++ [x : ys | ys <- power xs]

Definition:
A1 : [] ++ ys = ys
A2 : (x : xs) ++ ys = x : (xs ++ ys)

R1 : rev [] = []
R2 : rev (x : xs) = rev xs ++ [x]

Lemma:
rev (xs ++ [x]) = x : rev xs
    for all elements x and lists xs

Proof.
we show the claim by structural induction on xs
Base case:
If xs [] then
    lhs = rev ([] ++ [x : []])
        = rev [x : []]          by A1
        = rev [] ++ [x]         by R2
        = [] ++ [x]             by R1
        = [x]                   by A1
    rhs = x : rev []
        = x : []                by R1
        = [x]

if xs = y : ys for some element y and list ys' then
    lhs = rev ((y : ys) ++ [x])
        = rev (y : (ys ++ [x]))      by  A2
        = rev (ys ++ [x]) ++ [y]     by  R2
        = (x : rev ys) ++ [y]        by I.H.
        = x : (rev ys ++ [y])        by A2
    rhs = x : rev (y : ys)
        = x : (rev ys ++ [y])        by R2
QED.