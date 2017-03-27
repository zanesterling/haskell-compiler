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


-- Evaluation
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = False
isVal t | isNum t = True
isVal _  = False

eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$>  (eval' t)
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
