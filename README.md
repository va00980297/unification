# Unification (Martelli & Montanari Algorithm)

This project implements first-order term unification in Haskell based on:

Martelli, A. & Montanari, U. (1982), *An Efficient Unification Algorithm*, ACM TOPLAS.

---

## Overview

Implements classical first-order unification using the six rules:

- Delete
- Decompose
- Conflict
- Orient
- Eliminate
- Occurs check

Produces a substitution map that makes two terms equal.

---

## Project Structure

src/
- Term.hs   -- Term definitions
- Unify.hs  -- Unification algorithm
- Main.hs   -- Test cases

---

## Build & Run

cabal build
cabal run

---

## Example

f(X, b) ≡ f(a, Y)

Result:
{ X → a, Y → b }

---

## Occurs Check

X ≡ f(X)

Result: failure

---

## Verification (Optional)

Install SWI-Prolog:

sudo apt install swi-prolog

Run:

swipl

Examples:

?- unify_with_occurs_check(f(X, b), f(a, Y)).
?- unify_with_occurs_check(X, f(X)).
?- unify_with_occurs_check(f(a), g(a)).

---

## Reference

Martelli & Montanari (1982)
An Efficient Unification Algorithm

---

## Author

https://github.com/va00980297