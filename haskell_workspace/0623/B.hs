-- main :: IO()
-- main = do
--     putStrLn "hello"
--     putStrLn "hello, world"
--     let x = 10
--     putStrLn ("hello, world" ++ show x)

import Graphics.Gloss

window = InWindow "snowflake" (500, 500) (20, 20)


d = 3.1415926545467 / 3

eval (x, y, a) [] = [(x, y)]
eval (x, y, a) ('+' : cs) = eval (x, y, a + d) cs
eval (x, y, a) ('-' : cs) = eval (x, y, a - d) cs
eval (x, y, a) (c : cs) = (x, y) : eval (x', y', a) cs
    where
        x' = x + cos a
        y' = y + sin a

picture = Line (eval (0, 0, 0) "F+F--F+F")

main :: IO ()
main = display window white picture


-- runghc.exe .\B.hs
