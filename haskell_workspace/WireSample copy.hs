-- name:
-- id:
-- acknowledgements:

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
  Pictures [ box x y (if k == 1 then red elseif k == 0 blue elseif k == 2 yellow else black)
           | (i, row) <- zip [0..] grid,
             (j, k)  <- zip [0..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]

next :: a -> b -> State -> State
next _ _ grid = [ [ 1 - k | k <- row ] | row <- grid ]


next ((x,y), state) game
  | state == stateHEAD      = stateTAIL
  | state == stateTAIL      = stateCONDUCTOR
  | state == stateCONDUCTOR = checkNeighbors ((x,y), state) game directions state
  | otherwise               = stateEMPTY



window :: Display
window = InWindow "Wire World" (1000, 500) (100, 100)


initialState :: State
initialState = [[1,0,1,0],[0,1,0,1]]
  
fps :: Int
fps = 2 -- frame per second

main :: IO ()
main = do
    args <- getArgs
    -- file : "xor.txt" <- getArgs
    -- grid <- readFile file
    grid <- readFile "demo"
    print (lines grid)
    simulate window black fps initialState draw next


stateEMPTY     = 3
stateHEAD      = 0
stateTAIL      = 1
stateCONDUCTOR = 2

nodeState :: Char -> Integer
nodeState char
  | char == '.' = stateEMPTY
  | char == 'h' = stateHEAD
  | char == '*' = stateCONDUCTOR
  | otherwise   = stateTAIL
