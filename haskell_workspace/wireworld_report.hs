{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Concurrent
import           Data.Maybe
import           System.Environment

type Point = (Int, Int)
type State = Integer
type Node  = (Point, State)
type Game  = [Node]

stateEMPTY     = 0
stateHEAD      = 1
stateTAIL      = 2
stateCONDUCTOR = 3



import System.Environment
import Graphics.Gloss

box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * 50,      y * 50),
                    (x * 50 + 45, y * 50),
                    (x * 50 + 45, y * 50 + 45),
                    (x * 50,      y * 50 + 45)])

-- fromIntegral 转int类型
draw :: [State] -> Picture
draw grid =
  Pictures [ box x y (if k == 1 then red else if k == 0 then blue else if k == 2 then yellow else black)
           | (i, row) <- zip [0..] grid,
             (j, k)  <- zip [0..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]









-- 给定一个坐标a，b和状态，返回grid中该元素的下一个状态？？？？
next :: Game -> [State]
next game = prepareData1 $ nextState game
  


prepareData1 :: Game -> [State]
prepareData1 game =
  concat [ makeRow1 (game !! y) y | y <- [0..length game - 1]]

makeRow1 :: Int -> Int -> [State]
makeRow1 row y =
  [(row !! x) | x <- [0..length row - 1]]







-- 读取add文本中的的文件，读取出来以后存入rawData中
-- rawData类型为数组类型，文件中每一行字符串为一个数组的一个元素
-- 调用get方法，获取到rawData的每一行，然后执行prepareData方法将字符串转为game实体
main :: IO ()
main = do
  rawData <- readFile "./add.txt"
  get (prepareData $ lines rawData)


-- 将状态输出位string格式，即将数字的状态， 转为符号
representation :: State -> String
representation node
  | node == stateHEAD      = "o"
  | node == stateTAIL      = "x"
  | node == stateCONDUCTOR = "-"
  | otherwise              = " "

-- 将node转为IO对象
--  fst (fst node) 获取到node的坐标，的x轴
-- 如果x为0，则输出换行符号 ++ node状态的符号
-- 如果x坐标不为0，则直接输出 node状态的符号
-- snd node node的状态
putNode :: Node -> IO ()
putNode node
  | fst (fst node) == 0 = putStr $ "\n" ++ representation (snd node)
  | otherwise           = putStr $ representation (snd node)

-- 清除屏幕上的打印的所有字符
clearScreen :: IO ()
clearScreen = putStr "\ESC[2]"

-- 将game实体转为IO对象
-- 从game中获取node节点打印以后
-- 清除屏幕
-- 线程睡眠500000
-- 递归调用get函数，打印下一个game实体
get :: Game -> IO ()
get game = do
  sequence_ [putNode node | node <- game]
  clearScreen
  threadDelay 500000
  get (nextState game)


-- 给定一个字符，返回该字符的代号，这个代号是认为规定的，比如本程序中有如下指代
-- stateEMPTY     = 0
-- stateHEAD      = 1
-- stateTAIL      = 2
-- stateCONDUCTOR = 3
nodeState :: Char -> Integer
nodeState char
  | char == 't' = stateTAIL
  | char == 'h' = stateHEAD
  | char == '#' = stateCONDUCTOR
  | otherwise   = stateEMPTY

-- 按索引获取值可以用 !! 符号：[1,2,3] !! 0 值为 1
-- 给定一行元素 "......................................"，和y，y代表rowData数组中第几个元素
-- 获row !! x，为获取到每一行中，每一个元素，即.。 
-- 所以，x表示每一行中，每一个字符的位置
-- y表示，一个文档中，该字符，即该节点，后面会把这个字符转化为节点，即该节点在rowData，整个文档的数组中是第几行
-- 给定一个字符串，该字符串在文档中占第几行，返回一个节点集合，即该字符串中每一个节点的集合
makeRow :: String -> Int -> [Node]
makeRow row y =
  [((x,y), nodeState $ row !! x) | x <- [0..length row - 1]]
 
-- 字符串转为game实体，game实体中包含所有的node节点
-- rawData !! y 获取到rawData中第y个元素，由于y是属于[0..length rawData - 1]从0到rawData-1
-- 所以该步骤能够获取到rawData数组中的单个元素
-- makeRow操作可以获取到每行的所有节点的集合
-- concat可以将一个文档中rawData中，所有行的节点拼接成一个大的集合，这个集合叫做game
-- ["......................................","h**************.......................","...............***....................","............*.*...*...................","...........***....*....****...........","h******...*.*.*.*.*..**....*..........",".......*.*.....***..*......*..........",".......*.*......*.**.......*....**....",".......*.*..................****..*...","........*........................*****","............................****..*...","h**************............*....**....","...............***.........*..........","............*.*...*........*..........","...........***....*....****...........","h******...*.*.*.*.*..**...............",".......*.*.....***..*.................",".......*.*......*.**..................",".......*.*............................","........*.............................","......................................"]
-- 对单个的字符调用makeRow操作，将

prepareData :: [String] -> Game
prepareData rawData =
  concat [ makeRow (rawData !! y) y | y <- [0..length rawData - 1]]

-- 设置方向，方便后面获取每个节点的上下左右8个相邻的节点用的，potin类型
directions :: [Point]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

-- 根据坐标point，从game中获取一个node，node的结构是（坐标，状态）
getNode :: Point -> Game -> Maybe Node
getNode pos [] = Nothing
getNode pos (((x,y), status) : rest)
  | pos == (x,y) = Just ((x,y), status)
  | otherwise = getNode pos rest

-- node 是由坐标和状态组成的，snd node意思是获取到状态
isHead :: Maybe Node -> Integer
isHead node = case node of
  Just node -> if snd node == stateHEAD
                then stateHEAD
                else stateCONDUCTOR
  Nothing   -> stateCONDUCTOR

-- 给定一个node，game，方向坐标，该节点的状态？？？？？？？
-- 如果该节点是头节点，返回头节点，如果方向为空，返回导线
-- 如果该节点是其他类型，则需要判断邻居是否为头节点，如果邻居为头节点，则返回该点也为头节点
-- dirs 某个节点8个方向的坐标集合
-- tail 从这8个方向中去掉第一个以后的所有节点集合
-- getNode (x + fst (head dirs), y + snd (head dirs)) game
-- head dirs，获取第一个方向坐标， fst获取该坐标的x轴，snd获取到该坐标的y轴
-- 分别对x，y加上方向坐标的权重，得到该点的邻近坐标
-- getNode 即获取到邻近的节点 
-- isHead 判断该节点是否为首节点，如果是，返回stateHead
checkNeighbors :: Node -> Game -> [Point] -> Integer -> Integer
checkNeighbors ((x,y), nodeState) game dirs isAny
  | isAny == stateHEAD = stateHEAD
  | null dirs          = stateCONDUCTOR
  | otherwise          = checkNeighbors
     ((x,y), nodeState)
     game
     (tail dirs)
     (isHead (getNode (x + fst (head dirs), y + snd (head dirs)) game))

-- 某个给定节点的下一个变化的状态
-- 给定一个节点node 
-- 该节点位头节点是，变为尾节点，该节点位为节点时，变为导体，该节点位导体的时候，判断邻接点，根据邻接点得到该节点的状态
nextNodeState :: Node -> Game -> State
nextNodeState ((x,y), state) game
  | state == stateHEAD      = stateTAIL
  | state == stateTAIL      = stateCONDUCTOR
  | state == stateCONDUCTOR = checkNeighbors ((x,y), state) game directions state
  | otherwise               = stateEMPTY

-- 接受一个node节点，一个game实体
-- 返回一个node节点变为下一个node的节点，其中坐标不变，状态通过nextNodeState去判断
makeNode :: Node -> Game -> Node
makeNode node game = (fst node, nextNodeState node game)

-- 下一个状态，接收一个game实体，返回一个game实体
-- 对game执行makeNode操作，将该game转化为下一个game
-- 然后通过map操作将该函数指向当前game的所有元素中。 妙啊，秒啊，秒啊，美しいアルゴリズムですね！
nextState :: Game -> Game
nextState game = map (`makeNode` game) game
-- nextState game = map (`makeNode` game) game





fps :: Int
fps = 2 -- frame per second

main :: IO ()
main = do
    args <- getArgs
    grid <- readFile "demo"
    simulate window black fps initialState draw next