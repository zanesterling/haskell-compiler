module Type where

data Type
  = TBool
  | TNat
  | TArr Type Type -- arrow type, a function
  deriving (Eq, Show)
