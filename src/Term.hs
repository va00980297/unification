module Term where

------------------------------------------------------------
-- Term in first-order logic.
-- Variables are represented as strings for simplicity.
-- Constants are treated as functions with an empty argument list,
-- e.g. the constant `a` is `Fun "a" []`.
------------------------------------------------------------

-- data --> create a totally new Type
data Term
  = Var String
  | Func String [Term]
  deriving (Eq, Ord, Read, Show)

-- type --> rename an existed Type
type EqSet = [(Term, Term)]

------------------------------------------------------------
---- Helpers
------------------------------------------------------------
arity :: Term -> Int
arity (Var _) = 0
arity (Func f arg) = length arg

isVar :: Term -> Bool
isVar (Var a) = True
isVar _ = False

isFunc :: Term -> Bool
isFunc (Func f arg) = True
isFunc _ = False

