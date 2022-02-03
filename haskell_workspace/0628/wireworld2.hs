type Coord  = (Int, Int)
type Grid c = Map Coord c

newtype Rule c a = Rule {
    rule :: Grid c -> Coord -> a
}

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