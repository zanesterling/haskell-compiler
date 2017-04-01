module Syntax where


-- AST
type Name = String

data Expr
  = Var Name
  | Lam Name Type Expr
  | App Expr Expr
  | Lit Lit
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)


-- Types
data Type
  = TBool
  | TInt
  | TArr Type Type -- arrow type, a function
  deriving (Eq, Show)
