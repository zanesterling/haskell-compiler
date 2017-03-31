module Check where

import Control.Monad.Except

import Syntax
import Type


-- Type checking
type Check a = Except TypeError a

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope
  deriving (Show)

check :: Expr -> Either TypeError Type
check = runExcept . typeof


typeof :: Expr -> Check Type
typeof x = case x of
  Lit (LInt  _) -> return TNat
  Lit (LBool _) -> return TBool

  App a b -> do
    ta <- typeof a
    tb <- typeof b
    case ta of
      TArr tx ty | tx == tb  -> return ty
                 | otherwise -> throwError $ Mismatch tb tx
      e -> throwError $ NotFunction ta

  e -> error $ "type check not implemented for: " ++ show e
