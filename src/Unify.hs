{-# LANGUAGE DataKinds #-}

module Unify where

import Control.Applicative (Alternative (empty))
import Term (EqSet, Term (Func, Var, funcArgs, funcArity, funcName))

------------------------------------------------------------
-- The algorithm repeatedly applies the following rules:
-- Swap -> Decompose -> Delete -> Eliminate
--
-- It stops when:
--   * the equation set is fully simplified (success)
--   * a conflict or occurs-check failure is detected (failure)
--
-- Returns:
--   Just EqSet  : unification succeeds
--   Nothing     : unification fails
------------------------------------------------------------
unify :: EqSet -> Maybe EqSet
unify set = do
  result <- applyRules set
  if result == set
    then Just result
    else unify result

applyRules :: EqSet -> Maybe EqSet
applyRules set =
  check set
    >>= (Just . simplify)
    >>= check
    >>= (Just . delete)
    >>= check
    >>= (Just . eliminate)

check :: EqSet -> Maybe EqSet
check set
  | conflict set || not (occursCheck set) = Nothing
  | otherwise = Just set

simplify :: EqSet -> EqSet
simplify set = decompose (swap set)

------------------------------------------------------------
-- Rule: Delete
--
-- s ≐ s  →  remove the equation.
--
-- Two identical terms are already unified, so the equation
-- contributes nothing and can be discarded.
------------------------------------------------------------
delete :: EqSet -> EqSet
delete [] = []
delete ((s, t) : xs)
  | s == t = delete xs
  | otherwise = (s, t) : delete xs

------------------------------------------------------------
-- Rule: Decompose
--
-- f(s1,...,sn) ≐ f(t1,...,tn)
--      ↓
-- s1 ≐ t1, ..., sn ≐ tn
--
-- Replace an equation between two function terms with equations
-- between their corresponding arguments.
------------------------------------------------------------
decompose :: EqSet -> EqSet
decompose [] = []
decompose ((f@(Func {funcArgs = args1}), g@(Func {funcArgs = args2})) : xs)
  | null args1 || null args2 =
      (f, g) : decompose xs
  | otherwise = zipTerms args1 args2 ++ decompose xs
decompose (eq : xs) = eq : decompose xs

------------------------------------------------------------
-- Pair corresponding arguments of two function terms.
------------------------------------------------------------
zipTerms :: [Term] -> [Term] -> EqSet
zipTerms [] [] = []
zipTerms (s : xs) (t : ys) = (s, t) : zipTerms xs ys
zipTerms _ _ = []

------------------------------------------------------------
-- Rule: Conflict
--
-- f(...) ≐ g(...)
--
-- Fail if:
--   * function symbols are different, or
--   * arities are different.
------------------------------------------------------------
conflict :: EqSet -> Bool
conflict [] = False
conflict ((Func {funcName = f, funcArity = n}, Func {funcName = g, funcArity = m}) : xs)
  | f == g && n == m = conflict xs
  | otherwise = True
conflict (_ : xs) = conflict xs

------------------------------------------------------------
-- Rule: Swap
--
-- t ≐ x
--      ↓
-- x ≐ t
--
-- Ensure that variables always appear on the left-hand side.
------------------------------------------------------------
swap :: EqSet -> EqSet
swap [] = []
swap (eq : xs) =
  case eq of
    (func@(Func {}), Var x) -> (Var x, func) : swap xs
    _ -> eq : swap xs

------------------------------------------------------------
-- Rule: Eliminate
--
-- x ≐ t, where x ∉ vars(t)
--
-- Generate the substitution {x ↦ t}, apply it to the remaining
-- equations, and keep the binding in the result.
------------------------------------------------------------
eliminate :: EqSet -> EqSet
eliminate set = go set []
  where
    go [] processed = processed
    go ((Var x, t) : xs) processed =
      go (sub xs (Var x) t) (sub processed (Var x) t ++ [(Var x, t)])
    go (eq : xs) processed = go xs (processed ++ [eq])

------------------------------------------------------------
-- Apply a substitution to every equation in the equation set.
------------------------------------------------------------
sub :: EqSet -> Term -> Term -> EqSet
sub [] _ _ = []
sub ((lhs, rhs) : xs) (Var x) t =
  (subTerm lhs (Var x) t, subTerm rhs (Var x) t)
    : sub xs (Var x) t

------------------------------------------------------------
-- Apply a substitution recursively to a term.
--
-- If the variable does not match, it remains unchanged.
------------------------------------------------------------
subTerm :: Term -> Term -> Term -> Term
subTerm (Var v) (Var x) t
  | v == x = t
  | otherwise = Var v
subTerm func@(Func {funcArgs = args}) (Var x) t =
  func
    { funcArgs =
        map (\s -> subTerm s (Var x) t) args
    }

------------------------------------------------------------
-- Rule: Occurs Check
--
-- x ≐ t
--
-- Fail if x occurs anywhere inside t.
------------------------------------------------------------
occursCheck :: EqSet -> Bool
occursCheck [] = True
occursCheck ((Var x, Func {funcArgs = args}) : xs)
  | notOccursIn (Var x) args = occursCheck xs
  | otherwise = False
occursCheck (_ : xs) = occursCheck xs

------------------------------------------------------------
-- Check whether a variable does not occur in a list of terms.
------------------------------------------------------------
notOccursIn :: Term -> [Term] -> Bool
notOccursIn _ [] = True
notOccursIn (Var x) (Var y : ys)
  | x /= y = notOccursIn (Var x) ys
  | otherwise = False
notOccursIn (Var x) (func@(Func {funcArgs = args}) : ys)
  | notOccursIn (Var x) args = notOccursIn (Var x) ys
  | otherwise = False
