module Rewrite where

import Term (Pos, Sub, Term (Func, Var, funcArgs, funcArity, funcName))

------------------------------------------------------------
-- Term Rewriting
--
-- A rewrite system consists of a collection of rewrite rules:
--
--   l → r
--
-- A rule can be applied when the left-hand side l matches a
-- subterm of a given term.

------------------------------------------------------------

rewrite :: Term -> Sub -> Maybe Term
rewrite t [] = Nothing

positions :: Term -> [Pos]
positions (Var x) = [[]]
positions Func {funcName = f, funcArity = n, funcArgs = args} =
  [] : positionsArgs [] n args

positionsArgs :: Pos -> Int -> [Term] -> [Pos]
positionsArgs pos 0 [] = []
positionsArgs [ind] n (t : rest) =
  map
    (ind :)
    (positions t ++ positionsArgs [ind] (n - 1) rest)
