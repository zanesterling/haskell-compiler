module Eval (
  runEval
) where

import Data.List
import Data.Maybe

import Syntax


-- Values
data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr --(Eval.Scope)

instance Show Value where
  show (VInt  x)  = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"


type Step = (Int, Expr)


-- Evaluation
oneReduction :: Expr -> Maybe Expr
oneReduction x = case x of
  _ -> Nothing

runEval :: Expr -> (Value, [Step])
runEval x = (VInt 0, [(0, x)])
