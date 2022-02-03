stateEMPTY     = 0
stateHEAD      = 1
stateTAIL      = 2
stateCONDUCTOR = 3

main :: IO ()
main = do
  rawData <- readFile "./demo"
  get (prepareData $ lines rawData)

representation :: State -> String
representation node
  | node == stateHEAD      = "██"
  | node == stateTAIL      = "▓▓"
  | node == stateCONDUCTOR = "░░"
  | otherwise              = "  "

putNode :: Node -> IO ()
putNode node
  | fst (fst node) == 0 = putStr $ "\n" ++ representation (snd node)
  | otherwise           = putStr $ representation (snd node)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

get :: Game -> IO ()
get game = do
  sequence_ [putNode node | node ← game]
  clearScreen
  threadDelay 500000
  get (nextState game)

nodeState :: Char -> Integer
nodeState char
  | char == '.' = stateEMPTY
  | char == 'h' = stateHEAD
  | char == '*' = stateCONDUCTOR
  | otherwise   = stateTAIL

makeRow :: String -> Int -> [Node]
makeRow row y =
  [((x,y), nodeState $ row !! x) | x ← [0..length row - 1]]

prepareData :: [String] -> Game
prepareData rawData =
  concat [ makeRow (rawData !! y) y | y ← [0..length rawData - 1]]
