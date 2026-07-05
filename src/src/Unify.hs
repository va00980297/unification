{-# LANGUAGE DataKinds #-}

module Unify where

import Data.List (sort)
import Data.Map (argSet)
import Data.Type.Equality (apply)
import Term (EqSet, Term (Func, Var), isFunc, isVar)

------------------------------------------------------------
-- Unify a set of equations.
-- It applies a sequence of transformation steps iteratively (swap, decompose,
-- delete, eliminate) to simplify the equation set until a fixpoint.
--
-- Returns:
--   * Just EqSet  - if unification succeeds
--   * Nothing     - if a conflict is detected or unification fails
------------------------------------------------------------
unify :: EqSet -> Maybe EqSet
unify [] = Just []
unify g
  | conflict g || not (check g) = Nothing
  | otherwise = unify (eliminate (simplify g))

simplify :: EqSet -> EqSet
simplify g = delete (decompose (swap g))

------------------------------------------------------------
-- Rule: Delete
-- s ≐ s  →  remove (two identical terms, nothing to do)
-- L is the output after deleting all identical terms, empty at beginning
------------------------------------------------------------
delete :: EqSet -> EqSet
delete [] = []
delete ((a, b) : xs)
  | a == b = delete xs
  | otherwise = (a, b) : delete xs

--------------------------------------------------------------
-- Rule: Decompose
-- f(s1..sn) ≐ f(t1..tn)  →  add s1≐t1, ..., sn≐tn
-- conflict -> decompose -> delete(again?)
------------------------------------------------------------
decompose :: EqSet -> EqSet
decompose [] = []
decompose ((Func a arga, Func b argb) : ss)
  | arga /= [] && argb /= [] = zipTerms arga argb ++ decompose ss
  | otherwise = (Func a arga, Func b argb) : decompose ss
decompose (x : xs) = x : decompose xs

zipTerms :: [Term] -> [Term] -> EqSet
zipTerms [] [] = []
zipTerms (a : as) (b : bs) = (a, b) : zipTerms as bs

--------------------------------------------------------------
-- Rule: Conflict
-- f(...) ≐ g(...)  where f ≠ g or incompatible arities →  Fail(False)
------------------------------------------------------------
conflict :: EqSet -> Bool
conflict [] = False
conflict ((Func a arga, Func b argb) : xs)
  | a == b && length arga == (length argb) = conflict xs
  | otherwise = True
conflict (_ : xs) = conflict xs

--------------------------------------------------------------
-- Rule: swap
-- t ≐ x  where t is not a variable  →  flip to x ≐ t
------------------------------------------------------------
swap :: EqSet -> EqSet
swap [] = []
swap ((a, b) : xs)
  | isVar b && isFunc a = (b, a) : swap xs
  | otherwise = (a, b) : swap xs

---------------------------------------------------------------
-- Rule: Eliminate
-- x ≐ t  where x ∉ vars(t)  →  apply {x↦t} everywhere, record binding
-- 
-- eliminate: pick one equation
-- → produce binding
-- → apply once
-- → remove equation
------------------------------------------------------------
eliminate :: EqSet -> EqSet
eliminate [] = []
eliminate ((Var a, b) : xs) =
  (Var a, b) : eliminate (sub xs (Var a) b)
eliminate (x : xs) = x : eliminate xs

-- Apply substitution to a term, recursively replacing all variables.
-- If a variable is not in the substitution, it is left unchanged.
sub :: EqSet -> Term -> Term -> EqSet
sub [] _ _ = []
sub ((a, b) : ss) (Var x) y = (subTerm a (Var x) y, subTerm b (Var x) y) : sub ss (Var x) y

subTerm :: Term -> Term -> Term -> Term
subTerm (Var t) (Var x) y
  | t == x = y
  | otherwise = Var t
subTerm (Func t arg) (Var x) y = Func t (map (\t -> subTerm t (Var x) y) arg)

---------------------------------------------------------------
-- Rule: Check
-- x ≐ t  where x ∈ vars(t)  →  Fail(False))
------------------------------------------------------------
check :: EqSet -> Bool
check [] = True
check ((Var a, Func b argb) : xs)
  | notArg (Var a) argb = check xs
  | otherwise = False
check (_ : xs) = check xs

notArg :: Term -> [Term] -> Bool
notArg _ [] = True
notArg (Var a) (Var b : bs)
  | a /= b = notArg (Var a) bs
  | otherwise = False