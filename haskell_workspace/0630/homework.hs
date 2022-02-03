
-- rewrite rules []    =   []
-- rewirte rules ('A' : cs) = "B-A-B" ++ rewrite rules cs
-- rewirte rules ('B' : cs) = "A-B-A" ++ rewrite rules cs
-- rewirte rules (c : cs) = c : rewrite rules cs



rewrite rules []    =   []
rewrite rules (c : cs) =
    case lookup c rules of
        Just s -> s ++ rewrite rules cs
        Nothing -> c : rewrite rules cs



power f 0 x = x
power f n x = f (power f (n-1) x)
rules = [('A', "B-A-B", ('B', "A+B+A"))]
picture =
    Line (eval (0,0,0)) (power (rewrite rules) 8 "B-A-B")

main :: IO ()
main = display window white picture