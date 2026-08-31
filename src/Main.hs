module Main where

import Data.Maybe (isNothing)
import Term (Term (Var), mkFunc)
import Unify (conflict, decompose, delete, eliminate, occursCheck, swap, unify)
import Match (match)

main :: IO ()
main = do
  testMatch

---------------------------------------------------------
-- assert
---------------------------------------------------------
assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual actual expected msg =
  if actual == expected
    then putStrLn ("PASS: " ++ msg)
    else do
      putStrLn ("FAIL: " ++ msg)
      putStrLn $ "  expected:\n    " ++ show expected
      putStrLn $ "  but got:\n    " ++ show actual

---------------------------------------------------------
-- Terms
---------------------------------------------------------
x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

a :: Term
a = mkFunc "a" 0 []

b :: Term
b = mkFunc "b" 0 []

c :: Term
c = mkFunc "c" 0 []

f1 :: Term
f1 = mkFunc "f" 1 [a]

f2 :: Term
f2 = mkFunc "f" 2 [a, b]

g2 :: Term
g2 = mkFunc "g" 2 [a, b]

---------------------------------------------------------
-- DELETE
---------------------------------------------------------
testDelete :: IO ()
testDelete = do
  print "========== TEST DELETE =========="
  assertEqual (delete [(a, a)]) [] "delete same const"
  assertEqual (delete [(a, x)]) [(a, x)] "const-var not deleted"
  assertEqual (delete [(f1, f1)]) [] "delete same func"
  assertEqual (delete [(x, x)]) [] "delete same var"

---------------------------------------------------------
-- CONFLICT
---------------------------------------------------------
testConflict :: IO ()
testConflict = do
  print "========== TEST CONFLICT =========="
  assertEqual (conflict [(f1, f2)]) True "conflict: different arity"
  assertEqual (conflict [(f2, g2)]) True "conflict: different function"
  assertEqual (conflict [(f1, f1)]) False "no conflict"

---------------------------------------------------------
-- CHECK
---------------------------------------------------------
testOccursCheck :: IO ()
testOccursCheck = do
  print "========== TEST occursCheck=========="
  assertEqual (occursCheck [(x, a)]) True "valid check"
  assertEqual (occursCheck [(x, mkFunc "f" 1 [x])]) False "occurs check"

---------------------------------------------------------
-- SWAP
---------------------------------------------------------
testSwap :: IO ()
testSwap = do
  print "========== TEST SWAP =========="
  assertEqual (swap [(f1, x)]) [(x, f1)] "swap needed"
  assertEqual (swap [(x, f1)]) [(x, f1)] "no swap"

---------------------------------------------------------
-- DECOMPOSE
---------------------------------------------------------
testDecompose :: IO ()
testDecompose = do
  print "========== TEST DECOMPOSE =========="
  assertEqual
    (decompose [(mkFunc "f" 2 [x, y], mkFunc "f" 2 [a, b])])
    [(x, a), (y, b)]
    "decompose function"
  assertEqual
    (decompose [(a, b)])
    [(a, b)]
    "no decomposition"

---------------------------------------------------------
-- ELIMINATE
---------------------------------------------------------
testEliminate :: IO ()
testEliminate = do
  print "========== TEST ELIMINATE =========="
  assertEqual
    (eliminate [(x, a), (x, mkFunc "f" 1 [y]), (y, y)])
    [(x, a), (a, mkFunc "f" 1 [y]), (y, y)]
    "elimination"
  assertEqual
    (eliminate [(mkFunc "f" 2 [a, x], b), (x, y), (a, Var "z")])
    [(mkFunc "f" 2 [a, y], b), (x, y), (a, Var "z")]
    "elimination"

---------------------------------------------------------
-- UNIFY
---------------------------------------------------------
testUnify :: IO ()
testUnify = do
  print "========== TEST UNIFY =========="

  -- Basic: variable unifies with constant
  -- x ≐ b, a ≐ y  →  {x ↦ b, y ↦ a}
  assertEqual
    (unify [(x, b), (a, y)])
    (Just [(x, b), (y, a)])
    "unify two var with const"

  -- Decompose then eliminate
  -- f(a, x) ≐ f(a, b)  →  {x ↦ b}
  assertEqual
    (unify [(mkFunc "f" 2 [a, x], mkFunc "f" 2 [a, b])])
    (Just [(x, b)])
    "unify two functions"

  -- Nested decomposition
  -- f(x, g(y)) ≐ f(a, g(b))  →  {x ↦ a, y ↦ b}
  assertEqual
    (unify [(mkFunc "f" 2 [x, mkFunc "g" 1 [y]], mkFunc "f" 2 [a, mkFunc "g" 1 [b]])])
    (Just [(x, a), (y, b)])
    "nested decomposition"

  -- Variable unifies with variable
  -- x ≐ y  →  {x ↦ y}  (or {y ↦ x}, depending on orientation)
  assertEqual
    (unify [(x, y)])
    (Just [(x, y)])
    "variable unifies with variable"

  -- Chained elimination: x ≐ y, y ≐ a  →  {x ↦ a, y ↦ a}
  assertEqual
    (unify [(x, y), (y, a)])
    (Just [(x, a), (y, a)])
    "chained variable elimination"

  -- Already unified: empty set
  assertEqual
    (unify [])
    (Just [])
    "empty equation set"

  -- Conflict: different function symbols
  -- f(a) ≐ g(a)  →  Nothing
  assertEqual
    (unify [(mkFunc "f" 1 [a], mkFunc "g" 1 [a])])
    Nothing
    "conflict: different function symbols"

  -- Conflict: different arity
  -- f(a) ≐ f(a, b)  →  Nothing
  assertEqual
    (unify [(mkFunc "f" 1 [a], mkFunc "f" 2 [a, b])])
    Nothing
    "conflict: different arity"

  -- Occurs check failure
  -- x ≐ f(x)  →  Nothing
  assertEqual
    (unify [(x, mkFunc "f" 1 [x])])
    Nothing
    "occurs check: x in f(x)"

  -- Occurs check failure nested
  -- x ≐ f(g(x))  →  Nothing
  assertEqual
    (unify [(x, mkFunc "f" 1 [mkFunc "g" 1 [x]])])
    Nothing
    "occurs check: x nested in f(g(x))"

  -- Multiple variables, one shared
  -- f(x, x) ≐ f(a, a)  →  {x ↦ a}
  assertEqual
    (unify [(mkFunc "f" 2 [x, x], mkFunc "f" 2 [a, a])])
    (Just [(x, a)])
    "same variable appears twice"

  -- Multiple variables, conflict via chaining
  -- x ≐ a, x ≐ b  →  Nothing  (a ≠ b)
  assertEqual
    (unify [(x, a), (x, b)])
    Nothing
    "conflict via chained elimination"

  ---------------------------------------------------------
-- MATCH
---------------------------------------------------------
testMatch :: IO ()
testMatch = do
  print "========== TEST MATCH =========="

  -- Pattern variable matches the same variable
  -- x ~ x  →  {}
  assertEqual
    (match x x)
    (Just [])
    "match same variable"

  -- Pattern variable matches a different variable
  -- x ~ y  →  {x ↦ y}
  assertEqual
    (match x y)
    (Just [(x, y)])
    "match variable with variable"

  -- Pattern variable matches a constant
  -- x ~ a  →  {x ↦ a}
  assertEqual
    (match x a)
    (Just [(x, a)])
    "match variable with constant"

  -- Pattern variable matches a function term
  -- x ~ f(a)  →  {x ↦ f(a)}
  assertEqual
    (match x f1)
    (Just [(x, f1)])
    "match variable with function"

  -- Same function symbol and arity
  -- f(x) ~ f(a)  →  {x ↦ a}
  assertEqual
    (match (mkFunc "f" 1 [x]) f1)
    (Just [(x, a)])
    "match same function"

  -- Function matching with multiple arguments
  -- f(x, y) ~ f(a, b)  →  {x ↦ a, y ↦ b}
  assertEqual
    (match (mkFunc "f" 2 [x, y]) f2)
    (Just [(x, a), (y, b)])
    "match function with multiple arguments"

  -- Nested function matching
  -- f(x, g(y)) ~ f(a, g(b))  →  {x ↦ a, y ↦ b}
  assertEqual
    (match
      (mkFunc "f" 2 [x, mkFunc "g" 1 [y]])
      (mkFunc "f" 2 [a, mkFunc "g" 1 [b]]))
    (Just [(x, a), (y, b)])
    "match nested functions"

  -- Different function symbols
  -- f(a) ~ g(a)  →  Nothing
  assertEqual
    (match f1 (mkFunc "g" 1 [a]))
    Nothing
    "match conflict: different function symbols"

  -- Different arity
  -- f(a) ~ f(a, b)  →  Nothing
  assertEqual
    (match f1 f2)
    Nothing
    "match conflict: different arity"

  -- Incompatible structures
  -- x is a pattern variable, so this one should succeed
  -- x ~ f(a)  →  {x ↦ f(a)}
  assertEqual
    (match x f1)
    (Just [(x, f1)])
    "match variable with function"

  -- Function cannot match a variable on the pattern side
  -- f(a) ~ x  →  Nothing
  assertEqual
    (match f1 x)
    Nothing
    "match function with target variable"

  -- Shared pattern variable
  -- f(x, x) ~ f(a, a)  →  {x ↦ a}
  assertEqual
    (match (mkFunc "f" 2 [x, x]) (mkFunc "f" 2 [a, a]))
    (Just [(x, a), (x, a)])
    "match same pattern variable twice"

  -- Shared pattern variable with different targets
  -- f(x, x) ~ f(a, b)
  --
  -- This case is important because the same pattern variable
  -- is matched against two different target terms.
  assertEqual
    (match (mkFunc "f" 2 [x, x]) (mkFunc "f" 2 [a, b]))
    (Just [(x, a), (x, b)])
    "match repeated variable with different targets"