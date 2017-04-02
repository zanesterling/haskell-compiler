{-# LANGUAGE FlexibleInstances #-}

module Eval (
  runEval
) where

import Control.Monad.State
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Text.PrettyPrint

import Pretty
import Syntax


-- Values
data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Pretty Value where
  ppr _ (VInt  x)          = text $ show x
  ppr _ (VBool x)          = text $ show x
  ppr p (VClosure v x env) =
       text "\\" <> text v <+> text "." <+> ppr (p+1) x
    <> if null env
         then text ""
         else text " |" <+> (hsep $ map (ppr 0) (Map.assocs env))

instance Pretty (String, Value) where
  ppr p (n, v) = text "(" <> text n <> text "," <+> ppr 0 v <> text ")"


type Scope = Map.Map String Value
emptyScope = Map.empty

type Step = (Int, Expr) -- (depth, partially evaluated expression)
type Eval a = WriterT [Step] (State EvalState) a


-- State and logging of evaluation
data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = depth s + 1}
  out <- m
  modify $ \s -> s { depth = depth s - 1}
  return out

record :: Expr -> Eval ()
record x = do
  d <- gets depth
  tell [(d,x)]
  return ()


-- Evaluation
eval :: Eval.Scope -> Expr -> Eval Value
eval scope x = case x of
  Lam n _ body -> inc $ do
    return $ VClosure n body scope

  App a b -> inc $ do
    x <- eval scope a
    record a
    y <- eval scope b
    record b
    appl x y

  Var n -> do
    record x
    return $ scope Map.! n

  Lit (LInt  a) -> return $ VInt (fromIntegral a)
  Lit (LBool a) -> return $ VBool a

appl :: Value -> Value -> Eval Value
appl (VClosure n e scope) x = do
  eval (Map.insert n x scope) e
appl _ _ = error "Tried to apply non-closure"


-- Interface
runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
