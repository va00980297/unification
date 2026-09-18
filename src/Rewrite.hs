module Rewrite where

import Term (Context, Sub, 
  Term (Func, Var, funcArgs, funcArity, funcName))
import Match (matchTerm,subTerm)
------------------------------------------------------------
-- Term Rewriting
--
-- Apply a rewrite rule l -> r to a term u.
--
-- Finds all subterms u' of u that match l, computes the
-- substitution sigma, and returns all possible results C[r*sigma].
--
-- Returns [] if no subterm matches.
------------------------------------------------------------
rewrite :: Term -> (Term, Term) -> [Term]
rewrite u (l,r)= tryContexts u (contexts u) (l,r)


------------------------------------------------------------
-- Try applying a rewrite rule to each (context, subterm) pair.
--
-- Skips pairs where the subterm does not match the rule lhs.
-- For successful matches, fills the context with r*sigma.
------------------------------------------------------------
tryContexts :: Term -> [(Context, Term)] -> (Term, Term) -> [Term]
tryContexts u [] _ = []
tryContexts u ((c,v):rest) (l,r) = 
  case matchTerm l v of 
    [] -> tryContexts u rest (l,r)
    sigma -> c (subTerm sigma r) : tryContexts u rest (l,r)



------------------------------------------------------------
-- Given a term, enumerate all (context, subterm) pairs such that
-- context applied to subterm reconstructs the original term.
--
-- C[u'] = u  for each (C[_], u') in the result.
------------------------------------------------------------
contexts :: Term -> [(Context, Term)]
contexts (Var x) = [(\hole -> hole, Var x)]
contexts func@(Func {}) =
  (\hole -> hole, func) : contextsArgs func 0

------------------------------------------------------------
-- Given a function term, enumerate all (context, subterm) pairs
-- arising from its argument list.
--
-- For each argument at index i, recursively find its contexts
-- and lift them into the parent.
------------------------------------------------------------
contextsArgs :: Term -> Int -> [(Context, Term)]
contextsArgs Func {funcArgs = []} _ = []
contextsArgs func@(Func {funcArgs = u : rest}) i = 
  contextsChild 
    func i (contexts u) ++ contextsArgs func {funcArgs = rest} (i + 1)

contextsChild :: Term -> Int -> [(Context, Term)] -> [(Context, Term)]
contextsChild _ _ [] = []
contextsChild func@(Func {}) i ((c, v):rest)= 
  (liftContext func i c, v) : contextsChild func i rest

------------------------------------------------------------
-- Used to lift a child context up to the parent level.
-- Given a parent term, the position and context of a child term.
------------------------------------------------------------
liftContext :: Term -> Int -> Context -> Context
liftContext func@(Func {funcArgs = args}) i childCtx =
  \hole -> func { funcArgs = replaceAt args i (childCtx hole) }

------------------------------------------------------------
-- Given a list of terms and an index i, replace the i-th
-- element with a hole, producing a context for the parent term.
------------------------------------------------------------
replaceAt :: [Term] -> Int -> Term -> [Term]
replaceAt [] _ _ = []
replaceAt (x:xs) 0 hole = hole : xs
replaceAt (x : xs) n hole = x : replaceAt xs (n - 1) hole

