module Rewrite where

import Term (Sub, Term (Func, Var, funcArgs, funcArity, funcName))

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
rewrite (Var x) (Var l,r):xs = 
    | l == x = Just r
    | otherwise = rewrite (Var x) xs
rewrite Func {funcName = f, funcArity=n,funcArgs = args}    
    (Func {funcName=l, funcArity = nl, funcArgs=argsl }, r):xs 
    | f == l && n == nl && args == argsl = Just rhs
    | otherwise == rewrite ()
rewrite t _:xs = rewrite t xs

-- strategy??
rewriteFunc :: Func -> Sub -> Maybe Term
rewriteFunc Func {funcName = f, funcArity=n,funcArgs = args}    
    (Func {funcName=l, funcArity = nl, funcArgs=argsl }, r):xs 

