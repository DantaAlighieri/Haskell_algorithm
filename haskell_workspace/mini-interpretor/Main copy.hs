import Data.List (nub)
import TRS
import Parser
import System.Environment


substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Con f) sigma = Con f 
substitute (App t1 t2) sigma =
  App (substitute t1 sigma) (substitute t2 sigma)

match :: Term -> Term -> Maybe Substitution
match s t = match' [] [(s, t)]


match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma (((Con f1), (Con f2)) : ps)
  | f1 == f2 = match' ((f1, (Con f2)) : sigma) ps 
match' sigma ((App (Con f1) t1, App (Con f2) t2) : ps)
  | f1 == f2 = match' sigma ((t1, t2) : ps)
  | otherwise = Nothing
match' sigma ((App t1 t2, App t3 t4) : ps) = match' sigma ((t1, t3) : (t2, t4) : ps)
match' sigma ((Var x, t) : ps) = case lookup x sigma of
  Just t' 
    | t' == t       -> match' sigma ps
  Nothing           -> match' ((x, t) : sigma) ps
  _                 -> Nothing
match' _ _ = Nothing


-- match' sigma ((Cons cons, t) : ps) = match' ((cons, t) : sigma) ps
  -- | f1 == "main" && f1 == f2 = match' sigma ((ts, us) : ps)
-- match' sigma ((App t1 (Var x), App t2 (Var y)) : ps)
--   | x == y = match' sigma ((t1, t2) : ps)
--   | x /= y = match' sigma ((Var x, Var y) : ps)
-- match' sigma ((App t1 (Var x), App t2 (Con y)) : ps) = match' sigma ((Var x, Con y) : ps)
-- match' sigma ((Con f, t) : ps) = case lookup f sigma of
--   Nothing           -> match' ((f, t) : sigma) ps
--   Just t' 
--     | t' == t       -> match' sigma ps
--   _                 -> Nothing



-- trs:[(Term, Term)]
rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot [] _ = Nothing
rewriteAtRoot ((tl, tr) : rs) ts = case (match tl ts) of 
  Just sigma -> Just (substitute tr sigma)
  Nothing   -> rewriteAtRoot rs ts
-- rewriteAtRoot ((t1, t2) : rs) (Var x) = case (match (Var x) t1) of 
--   Nothing   -> rewriteAtRoot rs (Var x)
--   Just sigma -> Just (substitute (Var x) sigma)
-- rewriteAtRoot ((t1, t2) : rs) (Con x) = case (match (Con x) t1) of 
--   Nothing   -> rewriteAtRoot rs (Con x)
--   Just sigma -> Just (substitute (Con x) sigma)
-- rewriteAtRoot ((t1, t2) : rs) ts = case (match ts t1) of 
--   Nothing   -> rewriteAtRoot rs ts
--   Just sigma -> Just (substitute ts sigma)


-- rewriteAtRoot _ _ = Nothing

--left innermost
rewrite :: TRS -> Term -> Maybe Term
rewrite rules (Var x) = rewriteAtRoot rules (Var x)
rewrite rules (Con f) = rewriteAtRoot rules (Con f)
rewrite rules (App a1 a2) = case rewrite rules a1 of
  Nothing -> case rewrite rules a2 of
    Nothing -> case rewriteAtRoot rules (App a1 a2) of
      Just t3 -> Just t3
      Nothing -> Nothing
    Just t2  -> Just (App a1 t2)
  Just t1 -> Just (App t1 a2)
-- rewrite rules (App (Con f) app) = rewriteAtRoot rules (App (Con f) app)
-- rewrite rules (App (Con f) (Var x)) = rewriteAtRoot rules (App (Con f) (Var x))
-- rewrite rules (App (Con f) (Con cons)) = rewriteAtRoot rules (App (Con f) (Con cons))


-- tf
-- 变量的时候  lookup rule

-- tf
-- 函数的时候  
-- 	    继续找，往里面找，找到最里面一层函数的时候，停止

nf1 :: TRS -> Term -> Term
nf1 rules t1 = case (rewrite rules t1) of
  Just t2  -> nf1 rules t2
  Nothing -> t1


-- ________________________________________2_________________________________________________________

substitute2 :: Term -> Substitution -> MarkedTerm
substitute2 (Var x) sigma
  | Just t <- lookup x sigma = NF t
  | otherwise                = MCon x
substitute2 (Con f) sigma = MCon f 
substitute2 (App t1 t2) sigma =
  MApp (substitute2 t1 sigma) (substitute2 t2 sigma)

rewriteAtRoot2 :: TRS -> Term -> MarkedTerm
rewriteAtRoot2 [] ts = NF ts 
-- rewriteAtRoot2 ((tl, tr) : rs) ts = substitute2 tr (Just (match tl ts))
-- 找到了替换 ，所有的规则都找不到，NF
rewriteAtRoot2 ((tl, tr) : rs) ts = case (match tl ts) of 
  Just sigma -> substitute2 tr sigma
  Nothing    -> rewriteAtRoot2 rs ts


rewrite2 :: TRS -> MarkedTerm -> MarkedTerm
-- rewrite rules (Var x) = rewriteAtRoot rules (Var x)
-- 单个函数或者变量的情况：替换 适用于main 
rewrite2 rules (MCon f) = rewriteAtRoot2 rules (Con f)
rewrite2 rules (NF nf) = NF nf
-- mapp的情况:包含Nf的情况和不包含NF的情况
-- 2边都为NF的情况，扩大
rewrite2 rules (MApp (NF nf1) (NF nf2)) = rewriteAtRoot2 rules (App nf1 nf2)
-- 左边为NF 右边不是的时候，先rewrite mf，然后再rewrite 整个
-- rewrite2 rules (MApp (NF nf1) (MCon mf)) = rewriteAtRoot2 rules (Con mf)
-- 左边为Nf, 右边非NF，app
rewrite2 rules (MApp (NF nfl) xnf) = MApp (NF nfl) (rewrite2 rules xnf)
-- 左边为非NF，右边为Nf的情况，递归
rewrite2 rules (MApp xnf (NF nfr)) = MApp (rewrite2 rules xnf) (NF nfr)
-- 左右两边都不为nf的情况
rewrite2 rules (MApp a1 a2) = MApp (rewrite2 rules a1) (rewrite2 rules a2)



nf2 :: TRS -> MarkedTerm -> Term
nf2 rules mt = case (rewrite2 rules mt) of
  (NF nf) -> nf
  xnf     -> nf2 rules xnf

-- ________________________________________3_________________________________________________________

nf3 :: TRS -> MarkedTerm -> Term
-- nf3 rules (MApp (MApp a1 a2) (MApp b1 b2)) = App (nf3 rules (MApp a1 a2)) (nf3 rules (MApp b1 b2))
-- nf3 rules (MApp xnf1 xnf2) = App (nf3 rules xnf1) (nf3 rules xnf2)
-- nf3 rules (MCon f) = rewrite2 rules (MCon f)
nf3 rules (MCon f) = case (rewrite2 rules (MCon f)) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf
nf3 rules (MApp (NF app1) (NF app2)) = case (rewrite2 rules (MApp (NF app1) (NF app2))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf  
nf3 rules (MApp (NF app1) xnf) = case (rewrite2 rules (MApp (NF app1) (NF (nf3 rules xnf)))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf  
-- nf3 rules (MApp xnf (NF app1)) = case (rewrite2 rules (MApp (NF (nf3 rules xnf)) (NF app1))) of
--   (NF nf) -> nf
--   xnf     -> nf3 rules xnf    
nf3 rules (MApp xnf (NF app1)) = case (rewrite2 rules (MApp (NF (nf3 rules xnf)) (NF app1))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf    
-- nf3 rules (MApp t1 t2) = 
nf3 rules (MApp xnf1 xnf2) = case (rewrite2 rules (MApp (NF (nf3 rules xnf1)) (NF (nf3 rules xnf2)))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf
--   App (NF (rewrite2 rules t1)) (NF (rewrite2 rules t2))






-- nf3 rules (MCon f) = case (rewrite2 rules (MCon f)) of
--   (NF nf) -> nf
--   xnf     -> nf3 rules xnf
-- nf3 rules (MApp (NF app1) (NF app2)) = case (rewrite2 rules (MApp (NF app1) (NF app2))) of
--   (NF nf) -> nf
--   xnf     -> nf3 rules xnf  
-- nf3 rules (MApp (NF app1) (MApp a1 a2)) = case (rewrite2 rules (MApp (NF app1) (NF (nf3 rules (MApp a1 a2))))) of
--   (NF nf) -> nf
--   xnf     -> nf3 rules xnf  
-- nf3 rules (MApp (MApp a1 a2) (NF app1)) = case (rewrite2 rules (MApp (NF (nf3 rules (MApp a1 a2))) (NF app1))) of
--   (NF nf) -> nf
--   xnf     -> nf3 rules xnf    
-- nf3 rules (MApp xnf1 xnf2) = App (nf3 rules xnf1) (nf3 rules xnf2)



-- nf3 rules (MApp (NF app1) (NF app2)) = rewrite2 rules (MApp (NF app1) (NF app2))
-- nf3 rules (MApp (NF app1) xnf) = rewrite2 rules (MApp (NF app1) xnf)
-- nf3 rules (MApp xnf (NF app1)) = rewrite2 rules (MApp xnf (NF app1))
-- nf3 rules (MApp xnf1 xnf2) = App (nf3 rules xnf1) (nf3 rules xnf2)




main :: IO ()
main = do
  file : _ <- getArgs
  result <- readTRSFile file
  case result of
    Left e    -> print e
    Right trs -> do
      -- putStr (showTRS trs)                    -- Remove this when submitting your code.
      -- putStrLn "--"                           -- Remove this when submitting your code.
      -- print (readTerm "add (s X) Y")          -- Remove this when submitting your code.
      -- print (readTerm "s 0")                  -- Remove this when submitting your code.
      -- print (readTerm "1")                    -- Remove this when submitting your code.
      -- print (readTerm "cons 1 (cons 2 nil)")  -- Remove this when submitting your code.
      -- print (readTerm "[1,2]")                -- Remove this when submitting your code.
      -- putStrLn (showTRS (readTRS "main = add 1 2 . add (s X) Y = s (add X Y) .")) -- Remove this when submitting your code.
      -- putStrLn (show (nf1 trs (Con "main")))
      -- putStrLn (show (nf2 trs (MCon "main")))
      -- putStrLn (show (nf3 trs (MCon "main")))
      putStrLn "--trs"  
      -- print (readTerm "main = sum 10 .")
      putStrLn (showTRS trs)
      -- putStrLn (show (nf1 trs (readTerm "main = sum 10 .")))
      -- putStrLn (show trs (Con "main"))
      -- print (trs)
      -- putStrLn (show (nf1 trs (Con "main" )))
      -- print (nf1 trs (Con "main" ))
      -- putStrLn "--2"  
      -- putStrLn (show (nf2 trs (MCon "main")))
      -- putStrLn (show (nf1 trs (Con "main" )))
      putStrLn "--3"  
      putStrLn (show (nf3 trs (MCon "main")))
