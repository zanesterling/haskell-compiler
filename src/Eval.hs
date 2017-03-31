module Eval (
  runEval
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Pretty
import Syntax


-- Values
data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Show Value where
  show (VInt  x)  = show x
  show (VBool x)  = show x
  show (VClosure v x env) =
       ppexpr (Lam v x)
    ++ if null env then "" else " | " ++ show (Map.assocs env)

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
  Var n -> do
    record x
    return $ scope Map.! n

  Lam n body -> inc $ do
    return $ VClosure n body scope

  App a b -> inc $ do
    x <- eval scope a
    record a
    y <- eval scope b
    record b
    appl x y

  Lit (LInt  a) -> return $ VInt (fromIntegral a)
  Lit (LBool a) -> return $ VBool a

appl :: Value -> Value -> Eval Value
appl (VClosure n e scope) x = do
  eval (Map.insert n x scope) e
appl _ _ = error "Tried to apply non-closure"


-- Interface
runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
