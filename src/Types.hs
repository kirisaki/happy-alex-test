module Types where

data Exp
  = Apply Exp Exp
  | Lambda String Exp
  | Var String
  | Lit Lit

data Lit = LitInt Int deriving (Show)

instance Show Exp where
  show (Apply x y) = "(Apply" <> show x <> show y <> ")"
  show (Lambda x exp) = "(Lambda \"" <> x <> "\" " <> show exp <> ")"
  show (Var x) = "(Var \"" <> x <> "\")"
  show (Lit v) = "(Lit " <> show v <> ")"

data Type
  = TyVar String
  | TyInt
  | TyFun Type Type
  deriving (Show, Eq, Ord)

