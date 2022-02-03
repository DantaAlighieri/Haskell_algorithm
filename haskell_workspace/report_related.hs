import Data.List

-- 术语term类型，变量的时候为变量值的字符串，函数的时候，为函数名称字符串加术语
data Term = V String | F String [Term]

instance Show Term where
    show (V x) = x
    show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

-- intercalate取两个List作参数。将第一个List插入第二个List中间，并返回一个List.
-- ghci> intercalate " " ["i","am","happy"]   
-- "i am happy"  

-- >>> F "add" [F "s" [V "x"], V "y"]
-- add(s(x),y)
--

-- s(add(x,y))


-- >>> F "s" [F "add" [V "x", V "y"]]
-- s(add(x,y))
--

-- 获取变量
variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]


-- >>> variables (F "add" [F "s" [V "x"], V "y"])
-- ["x","y"]
--


data Exp = Val Int | Add Exp Exp | Mul Exp Exp
eval :: Exp -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- >>> eval (Mul (Val 10) (Add (Val 20) (Val 30)))
-- 500
--


-- type Substitution = [(String , Term )]
-- match :: Term -> Term -> Maybe Substitution
-- match V x F f ts = 

data Subst = [(String, Term)]
type Rule = (Term, Term)

substitute :: Term -> Subst -> Term
substitute (V x) sigma = ...
substitute (F f ts) sigma = ...
