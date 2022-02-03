-- name: CHEN LUWEI
-- id:2110112
-- acknowledgements:https://en.wikipedia.org/wiki/Wireworld

import System.Environment
import Graphics.Gloss

type State = [String]
data MyPoint = Square Char Int Int

--Polygon用来绘制多边形(一次控制四个正方形),Polygon是Picture的构造，相当于Node和Tree的关系，所以Polygon最终返回一个Picture.参数是Path，Path就是一些点的集合，把这些点连成线就是Path
box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * 50,      y * 50),  
                    (x * 50 + 45, y * 50),
                    (x * 50 + 45, y * 50 + 45),
                    (x * 50,      y * 50 + 45)])

--A function to convert the model to a picture.
draw :: State -> Picture
draw grid =  --这里的grid并不是直接从main那边过来的（也没办法直接设置全局变量），这里的grid是作为draw函数的State参数，而State参数就是initialState，这也是为什么老师一开始给的样本和ＴＸＴ文档完全无关
  Pictures [ box x y (if k == '#' then yellow else if k == 'h' then blue else if k == 't' then red else black) 
           | (i, row) <- zip [0..] grid, --先把一行行分开保存
             (j, k)  <- zip [0..] row, --再对每一行进行处理
             let x = fromIntegral j,--用来确定符号的位置
             let y = fromIntegral (-i) -- fromIntegral takes an integral number and turns it into a more general number. 
           ] --这整个是一个List comprehension

updateState :: Char -> Int -> Int -> State -> Char
updateState k i j grid | k == 'h' = 't'
                | k == 't' = '#'
                | k == '#' && elem (getNeighbours i j grid)  [1..2] = 'h'
                | otherwise = k

--　状态变化(更新规则)
next :: a -> b -> State -> State
next _ _ grid = [ [ updateState k i j grid 
                | (j, k)  <- zip [0..] row] 
                | (i, row) <- zip [0..] grid]

getNeighbours :: Int -> Int -> State -> Int
getNeighbours i j grid = length [ (Square k i' j')  | (i',j') <- [ (i'', j'') | i'' <- [i-1..i+1], j'' <- [j-1..j+1], (i'',j'') /= (i,j)],
                                                      let k = (grid !! i' !! j') , k == 'h']


--getNeighbours i j grid = length [ (Square k i' j')  | (i', row) <- zip [0..] grid,  (j', k)  <- zip [0..] row, 
--                                                      elem (i', j') [ (i'', j'') | i'' <- [i-1..i+1], j'' <- [j-1..j+1], (i'',j'') /= (i,j)],
--                                                      k == 'h']

--窗口的设定，不用更改
window :: Display
window = InWindow "Wire World" (1000, 500) (100, 100)

initialState :: State -> State
initialState grid = grid
  
fps :: Int
fps = 5 -- frame per second

main :: IO ()
main = do
  file : _ <- getArgs
  grid <- readFile file
  print (lines grid) --在terminal里打印
  simulate window black fps (initialState (lines grid))  draw next
