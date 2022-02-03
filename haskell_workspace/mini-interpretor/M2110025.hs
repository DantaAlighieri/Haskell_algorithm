-- name: Wang Yao
-- id: s2110025
-- acknowledgements: https://hackage.haskell.org/package/term-rewriting-0.2/docs/src/Data-Rewriting-Substitution-Match.html
-- http://www-sop.inria.fr/members/Martin.Avanzini/slides/avanzini-wst13.pdf
-- https://github.com/mzini/TcT/blob/master/examples/RaML/mergesort.raml.trs
-- https://stackoverflow.com/questions/50473205/how-do-i-use-if-then-else-statement-with-no-else-condition-in-haskell
-- https://stackoverflow.com/questions/8905272/convert-maybe-int-to-int-in-haskell
-- https://livebook.manning.com/concept/haskell/maybe-int
-- https://stackoverflow.com/questions/34533516/not-in-scope-data-constructor-cons
-- https://hackage.haskell.org/package/term-rewriting-0.2/docs/Data-Rewriting-Rules-Rewrite.html
-- https://hackage.haskell.org/package/term-rewriting-0.4.0.2/docs/src/Data.Rewriting.Substitution.Match.html#match
-- https://stackoverflow.com/questions/8749675/haskell-non-exhaustive-patterns-in-function


import Data.List (nub)
import TRS
import Parser
import System.Environment

data MarkedTerm = MApp MarkedTerm MarkedTerm | MCon String | NF Term
  deriving (Eq, Show)

-- ________________________________________1_________________________________________________________

substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Con f) sigma = Con f 
substitute (App t1 t2) sigma =
  App (substitute t1 sigma) (substitute t2 sigma)

match :: Term -> Term -> Maybe Substitution
match s t = match' [] [(s, t)]

-- term match 
match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
-- empty -> empty
match' sigma [] = Just sigma
-- conf f1 conf f2  -> check if both of them are the same. if so, add sigma and match the rest
match' sigma (((Con f1), (Con f2)) : ps)
  | f1 == f2 = match' ((f1, (Con f2)) : sigma) ps 
-- App app1 App app2 --> the same as the upper logic
match' sigma ((App (Con f1) t1, App (Con f2) t2) : ps)
  | f1 == f2 = match' sigma ((t1, t2) : ps)
  | otherwise = Nothing
-- app t1 t2, t3 t4, match t1 and t2, t3 and t4
match' sigma ((App t1 t2, App t3 t4) : ps) = match' sigma ((t1, t3) : (t2, t4) : ps)
-- var --> check if it is in sigma, if so match the rest, if not add to sigma and match the rest
match' sigma ((Var x, t) : ps) = case lookup x sigma of
  Just t' 
    | t' == t       -> match' sigma ps
  Nothing           -> match' ((x, t) : sigma) ps
  _                 -> Nothing
match' _ _ = Nothing

-- trs:[(Term, Term)]
-- If matched substitute, If not, next rule
rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot [] _ = Nothing
rewriteAtRoot ((tl, tr) : rs) ts = case (match tl ts) of 
  Just sigma -> Just (substitute tr sigma)
  Nothing   -> rewriteAtRoot rs ts

--left innermost
rewrite :: TRS -> Term -> Maybe Term
-- var rewrite root
rewrite rules (Var x) = rewriteAtRoot rules (Var x)
-- Con rewrite root
rewrite rules (Con f) = rewriteAtRoot rules (Con f)
-- Con rewrite App (app1 app2)
-- left innermost : if left not found --> right.
-- If right found, return. If not found, expand the scope(App a1 a2) and rewrite again
rewrite rules (App a1 a2) = case rewrite rules a1 of
  Nothing -> case rewrite rules a2 of
    Nothing -> case rewriteAtRoot rules (App a1 a2) of
      Just t3 -> Just t3
      Nothing -> Nothing
    Just t2  -> Just (App a1 t2)
  Just t1 -> Just (App t1 a2)

nf1 :: TRS -> Term -> Term
nf1 rules t1 = case (rewrite rules t1) of
  Just t2  -> nf1 rules t2
  Nothing -> t1


-- ________________________________________2_________________________________________________________

-- App 2 MApp， Con 2 Mcon， Var 2 MCon
substitute2 :: Term -> Substitution -> MarkedTerm
substitute2 (Var x) sigma
  | Just t <- lookup x sigma            = NF t
  | otherwise                           = MCon x
substitute2 (Con f) sigma = MCon f 
substitute2 (App t1 t2) sigma           = MApp (substitute2 t1 sigma) (substitute2 t2 sigma)

rewriteAtRoot2 :: TRS -> Term -> MarkedTerm
-- all rules not found. ->NF
rewriteAtRoot2 [] ts                    = NF ts 
-- found. ->substitue
rewriteAtRoot2 ((tl, tr) : rs) ts       = case (match tl ts) of 
  Just sigma -> substitute2 tr sigma
  Nothing    -> rewriteAtRoot2 rs ts


rewrite2 :: TRS -> MarkedTerm -> MarkedTerm
-- single f  subtitute also suitable for "main" 
rewrite2 rules (MCon f)                 = rewriteAtRoot2 rules (Con f)
rewrite2 rules (NF nf)                  = NF nf
-- mapp: NF and NF --solution--> expand scope App (nf1 nf2)
rewrite2 rules (MApp (NF nf1) (NF nf2)) = rewriteAtRoot2 rules (App nf1 nf2)
-- mapp: Left NF, Right XNF --solution--> rewrite Right XNF
rewrite2 rules (MApp (NF nfl) xnf)      = MApp (NF nfl) (rewrite2 rules xnf)
-- mapp: Left XNF, Right NF --solution--> rewrite Left XNF
rewrite2 rules (MApp xnf (NF nfr))      = MApp (rewrite2 rules xnf) (NF nfr)
-- 左右两边都不为nf的情况
-- mapp: Left XNF, Right XNF --solution--> (rewrite Left XNF and Right XNF)
rewrite2 rules (MApp a1 a2)             = MApp (rewrite2 rules a1) (rewrite2 rules a2)

nf2 :: TRS -> MarkedTerm -> Term
nf2 rules mt = case (rewrite2 rules mt) of
-- If NF --> nf
  (NF nf) -> nf
-- If not --> recursive
  xnf     -> nf2 rules xnf

-- ________________________________________3_________________________________________________________

nf3 :: TRS -> MarkedTerm -> Term
-- Mcon f --solution--> rewrite f
-- If return NF -> nf, If not -> recursive
nf3 rules (MCon f) = case (rewrite2 rules (MCon f)) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf
-- mapp: NF and NF --solution--> expand scope MApp (nf1 nf2)
nf3 rules (MApp (NF app1) (NF app2))    = case (rewrite2 rules (MApp (NF app1) (NF app2))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf  
-- mapp: NF and xnf --solution--> rewrite (NF, (recurisive XNF))
nf3 rules (MApp (NF app1) xnf)          = case (rewrite2 rules (MApp (NF app1) (NF (nf3 rules xnf)))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf
-- mapp: xnf and NF --solution--> rewrite ((recurisive XNF), NF)
nf3 rules (MApp xnf (NF app1))          = case (rewrite2 rules (MApp (NF (nf3 rules xnf)) (NF app1))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf   
-- mapp: xnf1 and xnf2 --solution--> rewrite ((recurisive XNF1), (recurisive XNF2))
nf3 rules (MApp xnf1 xnf2)              = case (rewrite2 rules (MApp (NF (nf3 rules xnf1)) (NF (nf3 rules xnf2)))) of
  (NF nf) -> nf
  xnf     -> nf3 rules xnf

main :: IO ()
main = do
  file : _ <- getArgs
  result <- readTRSFile file
  case result of
    Left e    -> print e
    Right trs -> do
      -- putStrLn (show (nf1 trs (Con "main")))
      -- putStrLn (show (nf2 trs (MCon "main")))
      putStrLn (show (nf3 trs (MCon "main")))
