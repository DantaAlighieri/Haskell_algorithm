import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen)

data Cell = Head | Tail | Wire
    deriving (Eq, Show)

type World = Map.Map (Int, Int) Cell

stepCell :: Cell -> [Cell] -> Cell
stepCell Head _ = Tail -- Heads always become tails
stepCell Tail _ = Wire -- Tails always become wires
stepCell Wire neighbours = -- Wires sometimes become Heads
    if numHeads == 1 || numHeads == 2 then Head else Wire
    where numHeads = length (filter (\n -> n == Head) neighbours)
    

cellNeighbours :: (Int, Int) -> World -> [Cell]
cellNeighbours (x, y) world = 
    catMaybes (foldl (\acc key -> Map.lookup key world : acc) [] possCord)
    where possCord = [(a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], (a, b) /= (x, y)]

step :: World -> World
step world = Map.mapWithKey (\key cell -> let neighbours = cellNeighbours key world in stepCell cell neighbours) world

prettyPrint :: World -> String
prettyPrint world = unlines (header : (map showRow viewport) ++ [header])
    where
        header = ['-' | _ <- [0..42]]

        showRow :: [(Int, Int)] -> String
        showRow coords = "|" ++ concatMap (\coord -> showCell (getCellAt coord world)) coords ++ "|"

        showCell Nothing = " "
        showCell (Just Head) = "X"
        showCell (Just Tail) = "O"
        showCell (Just Wire) = "-"

        getCellAt = Map.lookup
        viewport = [
            [(a, b) | a <- [-10..30]]
                    | b <- [1..16]]

runStep :: World -> IO ()
runStep world = do
    -- clearScreen
    putStr . prettyPrint $ world
    threadDelay (floor 2e5) -- Sleep for half a second
    runStep $ step world

main = do
    runStep initWorld
    where
        generator first = [ -- Hacky "generator" generator for base output
            ((0, 6), Tail),
            ((1, 5), Head),
            ((2, 5), Wire),
            ((3, 5), Wire),
            ((4, 5), Wire),
            ((5, 5), Wire),
            ((6, 5), if first then Tail else Wire),
            ((7, 5), if first then Head else Wire),
            ((8, 5), Wire),
            ((1, 7), Wire),
            ((2, 7), Wire),
            ((3, 7), Wire),
            ((4, 7), Wire),
            ((5, 7), if first then Wire else Head),
            ((6, 7), if first then Wire else Tail),
            ((7, 7), Wire),
            ((8, 7), Wire),
            ((9, 6), Wire),
            ((10, 6), Wire),
            ((11, 6), Wire),
            ((12, 6), Wire),
            ((13, 6), Wire),
            ((14, 6), Wire),
            ((15, 6), Wire)
            ]
        xor = [((16, 7), Wire)
            ,((16, 11), Wire)
            ,((15, 8), Wire)
            ,((15, 9), Wire)
            ,((15, 10), Wire)
            ,((16, 8), Wire)
            ,((16, 10), Wire)
            ,((17, 8), Wire)
            ,((17, 10), Wire)
            ,((18, 8), Wire)
            ,((18, 10), Wire)
            ,((18, 9), Wire)
            ,((19, 9), Wire)
            ,((20, 9), Wire)
            ,((21, 9), Wire)
            ,((22, 9), Wire)
            ,((23, 9), Wire)
            ,((24, 9), Wire)
            ,((25, 9), Wire)
            ,((26, 9), Wire)
            ,((27, 9), Wire)
            ,((28, 9), Wire)
            ,((29, 9), Wire)
            ]
        initWorld = Map.fromList (generator True ++ xor ++ (map (\ ((x, y), cell) -> ((x, y+6), cell)) (generator False))) -- Yuk, I guess it's okay for a hacknight :)







        