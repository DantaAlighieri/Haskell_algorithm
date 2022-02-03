data Exp = Val Int | Var String | Add Exp Exp | Mul Exp Exp
type Env = [(String, Int)]




eval1 :: Env -> Exp -> Int



eval2 :: Env -> Exp -> Either String Int
