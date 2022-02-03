import Graphics.Gloss
-- picture = Line ((0,0), (100,0), (50, 100), (0, 0))
window = InWindow "Snowflake" (500, 500) (20, 20)
d = 3.14 / 3
-- eval '' (Float, Float ,Float) -> String -> [(Float, Float)]
eval (x, y, a) [] = [(x, y)]
eval (x, y, a) ('+' : cs) = eval (x, y, a + d) cs
eval (x, y, a) ('-' : cs) = eval (x, y, a - d) cs
eval(x, y, a) (c : cs) = (x, y) : eval(x', y', a) cs
    where
        x' = x + cos a
        y' = y + sin a
-- picture = Line (eval (0, 0, 0) "F+F--F+F")

rewrite [] =[]
rewrite ('F' : cs) = "F+F--F+F" ++ rewrite cs
rewrite (c : cs) = c: rewrite cs

-- picture = Line (eval (0, 0, 0) "F+F--F+F--F+F--F+F--F+F--F+F")

-- picture = Line (eval (0, 0, 0) "F--F--F")
picture = Line (eval (0, 0, 0) (rewrite "F--F--F"))


power :: (Eq t1, Num t1) => (t2 -> t2) -> t1 -> t2 -> t2
power f 0 x = x
power f n x = f (power f (n - 1) x)

main :: IO ()
main = display winod white picture