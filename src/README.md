# Unification (Martelli & Montanari)

This project implements first-order term unification based on the algorithm from
Martelli & Montanari (1982).

## File Description

- `Term.hs`           Data type definitions for terms
- `Unify.hs`          The six simplification rules and the main unification loop
- `Main.hs`           Test cases

## Run

```bash
cabal run
```

## Testing & Verification

Use SWI-Prolog as an oracle to verify your results independently.

**Install:**
```bash
# Ubuntu/Debian
sudo apt install swi-prolog
```

**Run:**
```bash
swipl
```

Prolog's `unify_with_occurs_check/2` is standard first-order unification — use it
to cross-check any test case from `Main.hs`. For each case, write the equivalent
query and confirm the results match.

```prolog
% Basic success — expect {x -> a, y -> b}
?- unify_with_occurs_check(f(X, b), f(a, Y)).

% Nested decomposition — expect {x -> a, y -> b}
?- unify_with_occurs_check(f(X, g(Y)), f(a, g(b))).

% Conflict (different function heads) — expect false
?- unify_with_occurs_check(f(a), g(a)).

% Occurs check — expect false
?- unify_with_occurs_check(X, f(X)).
```

Note: Prolog shows you the unified term with variables filled in; your Haskell
implementation returns a substitution `Map`. The underlying result is the same —
just translate between the two representations when comparing.

## Reference

- Martelli & Montanari, *An Efficient Unification Algorithm*, ACM TOPLAS 1982