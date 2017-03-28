module Syntax where

import Data.Maybe

-- AST
data Expr
  = BoolExpr BoolExpr
  | NumExpr NumExpr
  deriving (Eq, Show)

data BoolExpr
  = Tr
  | Fl
  | IsZero NumExpr
  | BoolIf BoolExpr BoolExpr BoolExpr
  deriving (Eq, Show)

data NumExpr
  = Zero
  | Succ NumExpr
  | Pred NumExpr
  | NumIf BoolExpr NumExpr NumExpr
  deriving (Eq, Show)


-- Evaluation
isNum :: NumExpr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal (BoolExpr Tr) = True
isVal (BoolExpr Fl) = True
isVal (NumExpr t) | isNum t = True
isVal _  = False

eval_bool :: BoolExpr -> Maybe BoolExpr
eval_bool bx = case bx of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> eval_num t
  BoolIf Tr t _             -> Just t
  BoolIf Fl _ f             -> Just f
  BoolIf t c a              -> (\t' -> BoolIf t' c a) <$> eval_bool t
  _ -> Nothing

eval_num :: NumExpr -> Maybe NumExpr
eval_num nx = case nx of
  Succ t                    -> Succ <$> (eval_num t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$>  (eval_num t)
  NumIf Tr t _              -> Just t
  NumIf Fl _ f              -> Just f
  NumIf t c a               -> (\t' -> NumIf t' c a) <$> eval_bool t
  _ -> Nothing

eval' :: Expr -> Maybe Expr
eval' x = case x of
  BoolExpr x' -> BoolExpr <$> eval_bool x'
  NumExpr  x' -> NumExpr  <$> eval_num x'

nf :: Expr -> Expr
nf x = fromMaybe x $ nf <$> eval' x

eval :: Expr -> Maybe Expr
eval x = case nf x of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
