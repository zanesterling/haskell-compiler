module Syntax where

-- AST
type Name = String

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
--  | Lit Lit
  deriving (Eq, Show)

{-
data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)
-}
