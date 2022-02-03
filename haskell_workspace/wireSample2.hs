-- name:
-- id:
-- acknowledgements:

module Main where

import System.Environment
import Graphics.Gloss
import           Control.Concurrent
import           Data.Maybe

type State = [[Int]]
-- type PositionXy = (Int, Int)
-- type StateSingle = Integer
-- type Node  = (PositionXy, StateSingle)
-- type Nodes  = [Node]

box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * 50,      y * 50),
                    (x * 50 + 45, y * 50),
                    (x * 50 + 45, y * 50 + 45),
                    (x * 50,      y * 50 + 45)])

-- fromIntegral 转int类型
draw :: State -> Picture
draw grid =
  Pictures [ box x y (if k == 1 then red else if k == 0 then blue else if k == 2 then yellow else black)
           | (i, row) <- zip [0..] grid,
             (j, k)  <- zip [0..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]


-- 给定一个坐标a，b和状态，返回grid中该元素的下一个状态？？？？
next :: a -> b -> State -> State
next _ _ grid = [ [ 1 - k | k <- row ] | row <- grid ]









-- prepareData1 :: State -> State
-- prepareData1 rawData =
--   concat [ makeRow1 (rawData !! y) y | y <- [0..length rawData - 1]]

-- makeRow1 :: String -> Int -> State
-- makeRow1 row y =
--   [(row !! x) | x <- [0..length row - 1]]

-- length_grid :: State -> Int
-- length_grid [[]] = 0
-- length_grid grid = length grid


-- length_row :: [] -> Int
-- length_row [] = []
-- length_row row = length 

-- 对这个state数组进行操作，0不变，1变成2，2变成0，3的情况下，看邻居，然后看是否需要变成1
-- stateEMPTY     = 0
-- stateHEAD      = 1
-- stateTAIL      = 2
-- stateCONDUCTOR = 3







-- nextNodeState :: State -> State
-- nextNodeState ((x,y), state) game
--   | state == stateHEAD      = stateTAIL
--   | state == stateTAIL      = stateCONDUCTOR
--   | state == stateCONDUCTOR = checkNeighbors ((x,y), state) game directions state
--   | otherwise               = stateEMPTY


-- checkNeighbors :: Node -> Game -> [Point] -> Integer -> Integer
-- checkNeighbors ((x,y), nodeState) game dirs isAny
--   | isAny == stateHEAD = stateHEAD
--   | null dirs          = stateCONDUCTOR
--   | otherwise          = checkNeighbors
--      ((x,y), nodeState)
--      game
--      (tail dirs)
--      (isHead (getNode (x + fst (head dirs), y + snd (head dirs)) game))


-- 根据坐标point，从game中获取一个node，node的结构是（坐标，状态）
-- getNode :: Point -> Game -> Maybe Node
-- getNode pos [] = Nothing
-- getNode pos (((x,y), status) : rest)
--   | pos == (x,y) = Just ((x,y), status)
--   | otherwise = getNode pos rest


-- node 是由坐标和状态组成的，snd node意思是获取到状态
-- isHead :: Maybe Node -> Integer
-- isHead node = case node of
--   Just node -> if snd node == stateHEAD
--                 then stateHEAD
--                 else stateCONDUCTOR
--   Nothing   -> stateCONDUCTOR












window :: Display
window = InWindow "Wire World" (1000, 500) (100, 100)


initialState :: State
initialState = [[1,0]]
  
fps :: Int
fps = 2 -- frame per second

main :: IO ()
main = do
    args <- getArgs
    -- file : "xor.txt" <- getArgs
    -- grid <- readFile file
    grid <- readFile "demo"

    -- state <- prepareData $ lines grid
    print (prepareData $ lines grid)


    -- simulate window black fps state draw next






-- 字符串转为game实体，game实体中包含所有的node节点
-- rawData !! y 获取到rawData中第y个元素，由于y是属于[0..length rawData - 1]从0到rawData-1
-- 所以该步骤能够获取到rawData数组中的单个元素
-- makeRow操作可以获取到每行的所有节点的集合
-- concat可以将一个文档中rawData中，所有行的节点拼接成一个大的集合，这个集合叫做game
-- ["......................................","h**************.......................","...............***....................","............*.*...*...................","...........***....*....****...........","h******...*.*.*.*.*..**....*..........",".......*.*.....***..*......*..........",".......*.*......*.**.......*....**....",".......*.*..................****..*...","........*........................*****","............................****..*...","h**************............*....**....","...............***.........*..........","............*.*...*........*..........","...........***....*....****...........","h******...*.*.*.*.*..**...............",".......*.*.....***..*.................",".......*.*......*.**..................",".......*.*............................","........*.............................","......................................"]
-- 对单个的字符调用makeRow操作，将
-- prepareData :: [String] -> [Int]
-- prepareData rawData =
--   concat [ makeRow (rawData !! y) y | y <- [0..length rawData - 1]]

-- 按索引获取值可以用 !! 符号：[1,2,3] !! 0 值为 1
-- 给定一行元素 "......................................"，和y，y代表rowData数组中第几个元素
-- 获row !! x，为获取到每一行中，每一个元素，即.。 
-- 所以，x表示每一行中，每一个字符的位置
-- y表示，一个文档中，该字符，即该节点，后面会把这个字符转化为节点，即该节点在rowData，整个文档的数组中是第几行
-- 给定一个字符串，该字符串在文档中占第几行，返回一个节点集合，即该字符串中每一个节点的集合
-- makeRow :: [String] -> Int -> [Int]
-- makeRow row y =
--   [((x,y), nodeState $ row !! x) | x <- [0..length row - 1]]

-- stateEMPTY     = 0
-- stateHEAD      = 1
-- stateTAIL      = 2
-- stateCONDUCTOR = 3

-- nodeState :: Char -> Int
-- nodeState char
--   | char == 't' = stateTAIL
--   | char == 'h' = stateHEAD
--   | char == '*' = stateCONDUCTOR
--   | otherwise   = stateEMPTY

prepareData :: [String] -> [[String]]
prepareData rawData =
  concat [ convert (rawData !! y) | y <- [0..length rawData - 1]]

convert :: String -> [[String]]
convert = map (map return) . lines


convertMapStrToMapInt :: [[String]] -> [[Int]]
convertMapStrToMapInt [[]] = [[]]
convertMapStrToMapInt xxs = [ [ x | x <- xs, changeState x ] | xs <- xxs] 

length_rows :: [[Int]] -> Int
length_rows [[]] = 0
length_rows arrays = length arrays

width_array :: [[Int]] -> Int
width_array rawData = length (head rawData)

stateEMPTY     = 0
stateHEAD      = 1
stateTAIL      = 2
stateCONDUCTOR = 3

changeState :: Char -> Int
changeState char
  | char == 't' = stateTAIL
  | char == 'h' = stateHEAD
  | char == '*' = stateCONDUCTOR
  | otherwise   = stateEMPTY
