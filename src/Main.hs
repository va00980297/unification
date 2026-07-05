module Main where

import Data.Maybe (isNothing)
import Term (Term (Func, Var))
import Unify (check, decompose, delete, eliminate, conflict, swap, unify)

main :: IO ()
main = do
  testDelete
  testConflict
  testCheck
  testSwap
  testDecompose
  testEliminate

---------------------------------------------------------
-- assert
---------------------------------------------------------

assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual actual expected msg =
  if actual == expected
    then putStrLn ("PASS: " ++ msg)
    else do
      putStrLn ("FAIL: " ++ msg)
      putStrLn ("  expected: " ++ show expected)
      putStrLn ("  but got : " ++ show actual)

---------------------------------------------------------
-- Terms
---------------------------------------------------------

x :: Term
x = Var "x"

y :: Term
y = Var "y"

a :: Term
a = Func "a" []

b :: Term
b = Func "b" []

f1 :: Term
f1 = Func "f" [a]

f2 :: Term
f2 = Func "f" [a, b]

g2 :: Term
g2 = Func "g" [a, b]

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

testCheck :: IO ()
testCheck = do
  print "========== TEST CHECK =========="
  assertEqual (check [(x, a)]) True "valid check"
  assertEqual (check [(x, Func "f" [x])]) False "occurs check"

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
    (decompose [(Func "f" [x, y], Func "f" [a, b])])
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
    (eliminate [(x, a), (x, Func "f" [y]), (y, y)])
    [(x, a), (a, Func "f" [y])]
    "eliminate"

---------------------------------------------------------
-- UNIFY
---------------------------------------------------------

testUnify :: IO ()
testUnify = do
  print "========== TEST UNIFY =========="
  assertEqual
    (unify [(x, b), (a, y)])
    (Just [(x, b), (y, a)])
    "unify two var with const"