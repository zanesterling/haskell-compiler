{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
  ppexpr,
  ppr,
  Pretty,
) where

import Syntax

import Text.PrettyPrint


class Pretty p where
  ppr :: Int -> p -> Doc

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

instance Pretty Name where
  ppr _ x = text x

instance Pretty Expr where
  ppr _ (Var n)   = text n
  ppr p (Lit (LInt  i)) = text $ show i
  ppr p (Lit (LBool b)) = text $ show b
  ppr p (Lam v _ b) = char '\\' <> ppr 0 v <+> text "." <+> ppr 0 b
  ppr p e@(App _ _) = parensIf (p>0) $ hsep args
    where args = map (ppr (p+1)) (reverse $ viewArgs e)

instance Pretty Type where
  ppr _ TBool        = text "Bool"
  ppr _ TInt         = text "Int"
  ppr p (TArr ta tb) =
    parensIf (p>0) $
          ppr (p+1) ta
      <+> text "->"
      <+> ppr p tb


-- Turn a chained application into a list of expressions.
viewArgs :: Expr -> [Expr]
viewArgs (App a b) = b : (viewArgs a)
viewArgs x = [x]

ppexpr :: Pretty a => a -> String
ppexpr = render . ppr 0
