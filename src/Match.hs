module Match where

import Term (Sub, Term (Func, Var, funcArgs, funcArity, funcName))

------------------------------------------------------------
-- Matching
--
-- Matching attempts to make a pattern term identical to a
-- target term by assigning variables that occur in the pattern.
--
-- Unlike unification:
--   * only pattern variables may be substituted
--   * the target term is never modified
--
-- The algorithm considers the following cases:
--   * variable vs. variable
--   * variable vs. arbitrary term
--   * function vs. function
--   * incompatible terms
--
-- Function terms are matched recursively by matching their
-- corresponding arguments.
--
-- Returns:
--   Just Sub : matching succeeds
--   Nothing  : matching fails
------------------------------------------------------------
match :: Term -> Term -> Maybe Sub
match (Var x) (Var y)
  -- (x, x) return empty, no extra redundance check needed
  | x == y = Just []
  | otherwise = Just [(Var x, Var y)]
match (Var x) t = Just [(Var x, t)]
match
  Func {funcName = f, funcArity = n, funcArgs = args1}
  Func {funcName = g, funcArity = m, funcArgs = args2}
    | f == g && n == m = matchArgs args1 args2
    | otherwise = Nothing
match _ _ = Nothing

------------------------------------------------------------
-- Match corresponding arguments of two function terms.
--
-- Each argument pair is matched independently, and the
-- resulting substitutions are merged incrementally.
-- Fails if any argument pair cannot be matched, or if
-- the combined substitutions are inconsistent.
------------------------------------------------------------
matchArgs :: [Term] -> [Term] -> Maybe Sub
matchArgs [] [] = Just []
matchArgs [] _ = Nothing
matchArgs _ [] = Nothing
matchArgs (x : xs) (y : ys) = do
  sigma <- match x y
  rest <- matchArgs xs ys
  merge sigma rest

------------------------------------------------------------
-- Merge two substitutions into one.
--
-- For each binding in the first substitution, check whether
-- it is consistent with the second. Bindings are added
-- incrementally via mergeSingle.
--
-- Fails if the same variable is bound to two different terms.
------------------------------------------------------------
merge :: Sub -> Sub -> Maybe Sub
merge [] sigma = Just sigma
merge sigma [] = Just sigma
merge ((p, t) : xs) sigma =
  case mergeSingle (p, t) sigma of
    Just tau -> merge xs tau
    Nothing -> Nothing

------------------------------------------------------------
-- Insert a single binding (p, t) into a substitution.
--
-- Three cases:
--   * p is not in the substitution: add (p, t)
--   * p is already bound to t:      keep as-is (consistent)
--   * p is already bound to s ≠ t:  fail (conflict)
------------------------------------------------------------
mergeSingle :: (Term, Term) -> Sub -> Maybe Sub
mergeSingle (p, t) [] = Just [(p, t)]
mergeSingle (p, t) ((q, s) : ys)
  | p == q && t == s = Just ((q, s) : ys)
  | p == q && t /= s = Nothing
  | p /= q =
      case mergeSingle (p, t) ys of
        Just rest -> Just ((q, s) : rest)
        Nothing -> Nothing