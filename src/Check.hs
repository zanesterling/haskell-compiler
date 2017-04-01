module Check (
  check,
  checkTop,
  TypeError(..)
) where

import Control.Monad.Except
import Control.Monad.Reader

import Syntax


-- Environment for type checking operations
type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend = (:)


-- Type checking
data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name
  deriving (Show)


type Check a = ExceptT TypeError (Reader Env) a

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local $ extend (x,t)

lookupVar :: Name -> Check Type
lookupVar v = do
  env <- ask
  case lookup v env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope v


check :: Expr -> Check Type
check x = case x of
  Lit LInt{}  -> return TInt
  Lit LBool{} -> return TBool

  Lam v t b -> TArr t <$> (inEnv (v,t) $ check b)

  App a b -> do
    ta <- check a
    tb <- check b
    case ta of
      TArr tx ty | tx == tb  -> return ty
                 | otherwise -> throwError $ Mismatch tb tx
      _ -> throwError $ NotFunction ta

  Var v -> lookupVar v


runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> Expr -> Either TypeError Type
checkTop env x = runCheck env $ check x
