module Syntax where

import Data.Maybe

-- AST
data Expr
  = Tr
  | Fl
  | Zero
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  deriving (Eq, Show)

data ExprType
  = Bool
  | Num
  deriving (Eq, Show)


-- Type checking
typecheck :: Expr -> Maybe ExprType
typecheck x = case x of
  Tr -> Just Bool
  Fl -> Just Bool
  Zero -> Just Num
  IsZero x' -> if typecheck x' == Just Num then Just Bool else Nothing
  Succ x'   -> if typecheck x' == Just Num then Just Num  else Nothing
  Pred x'   -> if typecheck x' == Just Num then Just Num  else Nothing
  If c t f  -> let tt = typecheck t in
    if typecheck c  == Just Bool && tt == typecheck f
    then tt
    else Nothing


-- Evaluation
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _  = False

eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval' t)
  If Tr t _                 -> Just t
  If Fl _ f                 -> Just f
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x $ nf <$> eval' x

eval :: Expr -> Maybe Expr
eval x = case nf x of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
