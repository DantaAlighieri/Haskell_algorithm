-- name: Wang Yao
-- id: s2110025
-- acknowledgements: Li Hanyuan, LI RUIZHI
-- reference: 
-- https://github.com/bergsans/cellular-automaton-in-haskell/blob/main/wireworld/WireWorld.hs
-- https://github.com/bradrn/cellular-automata
-- https://github.com/bollu/cellularAutomata
-- https://rosettacode.org/wiki/Wireworld#JavaScript

module Main where

import System.Environment
import Graphics.Gloss

type State = [[Int]]

box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * 50,      y * 50),
                    (x * 50 + 45, y * 50),
                    (x * 50 + 45, y * 50 + 45),
                    (x * 50,      y * 50 + 45)])

draw :: State -> Picture
draw grid =
  Pictures [ box x y (if k == 0 then (makeColor 0.8 0.8 0.8 1) else if k == 1 then blue else if k == 2 then red else yellow)
           | (i, row) <- zip [0..] grid,
             (j, k)  <- zip [0..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]

-- next state
next :: a -> b -> State -> State
next _ _ grid = nextState grid
  
-- return next state  
nextState :: State -> State
nextState grid =
  [getEveryRow (grid !! x) x grid | x <- [0..length grid - 1]]

-- iternate every state rows
getEveryRow :: [Int] -> Int -> State -> [Int]
getEveryRow row x grid = 
  [(getNextColor (row !! y) x y grid) | y <- [0..length row - 1]]

-- find the next color according to the current color
getNextColor :: Int -> Int -> Int -> State -> Int
getNextColor color x y grid 
    | color == 0 = 0
    | color == 1 = 2
    | color == 2 = 3
    | neighbourColor <= 2 && neighbourColor >= 1 = 1
    | otherwise = 3
  where neighbourColor = getColorByPositions (getPositions x y (length_rows grid) (width_array grid)) grid

-- get the node from the grid matrix by the position of (x,y)
getColorByPositions :: [(Int, Int)] -> State -> Int
getColorByPositions nps grid = length [value | (x, y)<- nps, let value = grid !! x !! y, value == 1]

-- get length of the 2d array
length_rows :: State -> Int
length_rows grid = length grid

-- get count of every element
width_array :: State -> Int
width_array grid = length (head grid)

-- every directions around the a node
directions :: [(Int, Int)]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

-- get every neighbour nodes of a node
getPositions :: Int -> Int -> Int -> Int -> [(Int, Int)]
getPositions x y x' y' = [(x + m, y + n) | (m,n) <- directions, x + m >= 0 && y + n >= 0 && x +m < x' && y + n < y']

window :: Display
window = InWindow "Wire World" (1000, 500) (100, 100)
  
fps :: Int
fps = 2 -- frame per second

main :: IO ()
main = do
    file : _ <- getArgs
    grid <- readFile file
    let initState = myMap2 $ lines grid
    simulate window black fps initState draw next

-- rules tail:2, head:1, #:3, empty:0
nodeState :: Char -> Int
nodeState char
  | char == 't' = 2
  | char == 'h' = 1
  | char == '#' = 3
  | otherwise   = 0

-- replace the char to rule number
myMap :: [Char] -> [Int]
myMap array1 = map nodeState array1

-- [string] to state
myMap2 :: [String] -> [[Int]]
myMap2 array2 = map (myMap) array2