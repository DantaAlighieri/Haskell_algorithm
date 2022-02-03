type State = [[Int]]


length_rows :: [[Int]] -> Int
length_rows [[]] = 0
length_rows arrays = length arrays

-- width_array :: [[Int]] -> Int
-- width_array [[]] = 0
-- width_array arrays = length $ fst arrays

width_array :: [[Int]] -> Int
width_array rawData = length (head rawData)

--- >>> listIsEqual [[1,2,3,4],[1]]
--- 4
---



-- prepareData :: [String] -> [[String]]
-- prepareData rawData =
--   concat [ convert (rawData !! y) | y <- [0..length rawData - 1]]

-- convert :: String -> [[String]]
-- convert = map (map return) . lines


-- convertMapStrToMapInt :: [[String]] -> [[Int]]



-- height_array :: [[Int]] -> Int
-- height_array [[]] = 0
-- height_array arrays = length arrays




-- convertMapStrToMapInt :: [[String]] -> [[Int]]
-- convertMapStrToMapInt [[]] = [[]]
-- convertMapStrToMapInt xxs = 





-- stateEMPTY     = 0
-- stateHEAD      = 1
-- stateTAIL      = 2
-- stateCONDUCTOR = 3

-- changeState :: String -> Int
-- changeState char
--   | char == "t" = stateTAIL
--   | char == "h" = stateHEAD
--   | char == "*" = stateCONDUCTOR
--   | otherwise   = stateEMPTY


-- myMap :: [String] -> [Int]
-- myMap [] = []
-- myMap array1 = map changeState array1


-- myMap2 :: [[String]] -> [[Int]]
-- myMap2 [[]] = [[]]
-- myMap2 array2 = map (map changeState) array2





-- nodeState :: String -> Int
-- nodeState char
--   | char == "t" = 2
--   | char == "h" = 1
--   | char == "*" = 3
--   | otherwise   = 0



-- myMap :: [String] -> [Int]
-- myMap array1 = map nodeState array1


-- myMap2 :: [String] -> [Int]
-- myMap2 array2 = map (nodeState) array2


-- >>> myMap2 ["******","h", "t"]
-- [0,1,2]
--


-- ["**","hh", "tt"]

-- [["*", "*"], ["h", "h"], ["t", "t"]]

transferTo2D :: [String] -> [[Char]]
transferTo2D [] = [[]]
transferTo2D (x : xs) = [x !! x_length | x_length <- [0..length x - 1]] : transferTo2D xs

-- >>> transferTo2D ["**","hh", "tt"]
-- ["**","hh","tt",""]
--



map1 :: [String] -> [[String]]
map1 [] = [[]]
map1 (x : xs) = [makeRow (x !! x_length) x_length | x_length <- [0..length x - 1]] : map1 xs



-- prepareData :: [String] -> [[Int]]
-- prepareData rawData =
--   makeRow (rawData !! y) y | y <- [0..length rawData - 1]



makeRow :: String -> Int -> [Char]
makeRow row y =
  [(row !! x) | x <- [0..length row - 1]]



-- getValue :: Int -> Int -> [[Int]] -> Int
-- getValue x y arr = arr !! x !! y

-- >>> getValue 1 2 [[1, 2, 3], [4, 5, 6]]
-- 6
--



directions :: [(Int, Int)]
-- directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

getPositions :: Int -> Int -> [(Int, Int)]
getPositions x y = [(x + m, y + n) | (m,n) <- directions]

-- >>> getPositions 0 0
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
--


getColorByPositions :: [(Int, Int)] -> [[Int]] -> [Int]
getColorByPositions [] [[]]= []
getColorByPositions [] _ = []
getColorByPositions _ [] = []

getColorByPositions ((x, y) : nps) arr =  (arr !! x !! y) : getColorByPositions nps arr

getColorByPositions2 :: [(Int, Int)] -> [[Int]] -> [Int]
getColorByPositions2 nps arr =  [arr !! x !! y | (x, y)<- nps]



getColorByPositions3 :: [(Int, Int)] -> [[Int]] -> Int
getColorByPositions3 nps arr = length [value | (x, y)<- nps, let value = arr !! x !! y, value == 1]

-- getColors :: [(Int, Int)] -> [[Int]] -> [Int]
-- getColors dirs arr = [arr !! ]

-- 错误方式
-- getColorByPositions2 nps arr =  [value | (x, y)<- nps, value <- [arr !! x !! y]]



-- >>> getColorByPositions3 [(0,1), (1,1)] [[1,2,3], [4,5,6], [7,8,9]]
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\testProject.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\testProject.hs:1:1: error:
--     The IO action emainf is not defined in module eMainf
-- Failed, no modules loaded.
--


getValue :: Int -> Int -> [[Int]] -> Int
getValue x y arr = arr !! x !! y









-- prepareData :: State -> State
-- prepareData grid =
--   [makeRow (grid !! x) x grid | x <- [0..length grid - 1]]

-- makeRow :: [Int] -> Int -> State -> State
-- makeRow row x grid = 
--   [(getNextColor (row !! y) x y grid) | y <- [0..length row - 1]]

-- prepareData :: [[Int]] -> [Int]
-- prepareData grid = [grid !! x | x <- [0..length grid - 1]]

-- >>> prepareData [[1,2,3], [4,5,6], [7,8,9]]
-- *** Exception: C:\haskell_workspace\testProject.hs:202:21-29: error:
--     ? Couldn't match expected type eIntf with actual type e[Int]f
--     ? In the expression: grid !! x
--       In the expression: [grid !! x | x <- [0 .. length grid - 1]]
--       In an equation for eprepareDataf:
--           prepareData grid = [grid !! x | x <- [0 .. length grid - 1]]
-- (deferred type error)
--

prepareData :: [[Int]] -> [Int]
prepareData grid = [x y | x <- [0..length grid - 1]]
