-- 問題[1]
Definition:
-- 空 加 列表 | 列表 加 列表
A1: [] ++ ys =ys for all ys
A2: (x : xs) ++ ys = x : (xs ++ ys) for all x, xs, ys

Theorem:
xs ++ [] = xs for all list xs.
Proof.
We show the claim by structureal induction on xs.
Base case:
If xs = [] then 
    lhs = [] ++ []
        = []              by A1
    rhs = []
If xs = (x : xs') then
    lhs = (x : xs') ++ []
        = x : (xs' ++ [])  by A2
        = x : xs'          by I.H.
    rhs = x : xs'        
QED.

-- ============================================================
-- 問題[2]
-- 空 反转 | 列表反转
A1: [] ++ ys =ys for all ys
A2: (x : xs) ++ ys = x : (xs ++ ys) for all x, xs, ys
Theorem:
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs for all list xs, ys, zs
Proof.
We show all the claim by structureal induction on xs.
Base case:
If xs = [] then
    lhs = [] ++ (ys ++ zs)
        = ys ++ zs                  by A1
    rhs = ([] ++ ys) ++ zs
        = ys ++ zs                  by A1
If xs = (x : xs) then
    lhs = (x : xs') ++ (ys ++ zs)
        = x : (xs' ++ (ys ++ zs))   by A2
        = x : ((xs' ++ ys) ++ zs )  by I.H.
    rhs = ((x : xs') ++ ys) ++ zs
        = (x : (xs' ++ ys) ++ zs)   by A2
        = x : (xs' ++ ys) ++ zs     by A2 
QED.






-- ============================================================

-- 問題[3]
Definition:
-- 空 加 列表 | 列表 加 列表
A1: [] ++ ys =ys
A2: (x : xs) ++ ys = x : (xs ++ ys)

-- 空 反转 | 列表反转
R1: rev [] = []
R2: rev (x : xs) = rev xs ++ [x]

-- 空 加 列表 反转 | 列表 加 列表 反转
RA1: revapp [] ys = ys
RA2: revapp (x : xs) : ys = revapp xs (x : ys)

Lemma:
revapp xs ys = rev xs ++ ys for all list xs and ys
Proof.
We show the claim by structureal induction on xs.
Base case:
If xs = [] then
    lhs = revapp [] ys
        = ys                           by RA1
    rhs = rev [] ++ ys
        = [] ++ ys                     by R1
        = ys                           by A1
If xs = x : xs' for some x and xs' then
    lhs = revapp (x : xs') ys
        = revapp xs' (x : ys)          by RA2
        = rev xs' ++ (x : ys)          by I.H.
    rhs = rev(x : xs') ++ ys
        = (rev xs' + [x]) ++ ys        by R2
        = rev xs' ++ ([x] ++ ys)       by associativeity of ++
        = rev xs' ++ (x : [] ++ ys)
        = rev xs' ++ (x : ([] ++ ys))  by A2
        = rev xs' ++ (x : ys)          by A1
QED.