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
match (Var p) (Var t)
    | p == t = Just []
    | otherwise = Just [(Var p, Var t)] 
match (Var p) t = Just [(Var p,t)] 
match Func {funcName = p, funcArity = np, funcArgs = argsp} Func {funcName = t, funcArity = nt, funcArgs = argst}
    | p == t && np == nt = 
        case flatten (zipWith match argsp argst) of
            Just sub -> merge sub
            Nothing -> Nothing
    | otherwise = Nothing
match _ _ = Nothing 

------------------------------------------------------------
-- Combine Matching Results
--
-- Match each pair of function arguments independently.
-- Each match produces either:
--
--   Just Sub : the pair matches successfully
--   Nothing  : the pair cannot be matched
--
-- Combine all successful substitutions into one substitution.
-- If any individual match fails, the entire match fails.
------------------------------------------------------------
flatten :: [Maybe Sub] -> Maybe Sub
flatten [] = Just []
flatten (Just x:xs) = 
    case (flatten xs) of 
        Just ys -> Just (x ++ ys)
        Nothing -> Nothing
flatten (Nothing:_) = Nothing 

-- !!!
merge :: Maybe Sub -> Maybe Sub
merge (Just []) = Just []
merge (Just (x:xs)) = 
    case checkBinding x xs of
        Just ys -> 
            case merge ys of 
                Just zs -> Just (x: zs)
                Nothing -> Nothing
        Nothing -> Nothing
merge Nothing = Nothing 

checkBinding :: (Term,Term) -> Sub -> Maybe Sub
checkBinding x [] = Just []
checkBinding (p,t) ((q,s):xs)
    | p == q && t == s = checkBinding (p,t) xs
    | p == q && t /= s = Nothing
    | p /= q = 
        case checkBinding (p,t) xs of
        Just ys -> Just ((q,s): ys) 
        Nothing -> Nothing
