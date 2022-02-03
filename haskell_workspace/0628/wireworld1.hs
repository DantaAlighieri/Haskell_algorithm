import Data.Map as Map
-- map
-- 取一个函数和list作为参数，遍历该list，将每个元素带入函数进行计算，得到一个新的list



type Coord  = (Int, Int)
type Grid c = Map Coord c

newtype Rule c a = Rule {
    rule :: Grid c -> Coord -> a
}

instance Monad (Rule c) where
    return = pure
    Rule r >>= f = Rule $ \ grid xy ->
        let
            a = r grid xy
            Rule f' = f a
        in
            f' grid xy

-- import qualified Data.Map as Map
-- myMap :: (a -> b) -> [a] -> [b]
-- myMap f [] = []
-- myMap f (x : xs) = f x : myMap f xs


runRule :: Rule c a -> Grid c -> Grid a
runRule (Rule r) grid = Map f grid
    where f xy _ = r grid xy

data Cell = Alive | Empty | Head | Tail | Conductor
          deriving (Eq, Show, Read, Enum)

wireWorld :: Rule Cell Cell
wireWorld = do
    s <- self
    n <- countAround (==Head)
    return $ case s of
        Conductor ->
            if n == 1 || n == 2 then
                Head
            else
                Conductor
        Head     -> Tail
        Tail     -> Conductor
        a        -> a