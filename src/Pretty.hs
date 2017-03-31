{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (
  ppexpr
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
  ppr p e@(Lam _ _) =
    parensIf (p>0) (char '\\' <> hsep vars <+> text "." <+> body)
    where
      vars = map (ppr 0) (viewVars e)
      body = ppr 0 (viewBody e)
  ppr p e@(App _ _) = parensIf (p>0) $ hsep args
    where args = map (ppr (p+1)) (reverse $ viewArgs e)


-- Get the varnames of a multi-arg function.
viewVars :: Expr -> [Name]
viewVars (Lam n x) = n : viewVars x
viewVars _ = []

-- Get the body of a multi-arg function.
viewBody :: Expr -> Expr
viewBody (Lam _ x) = viewBody x
viewBody x = x

-- Turn a chained application into a list of expressions.
viewArgs :: Expr -> [Expr]
viewArgs (App a b) = b : (viewArgs a)
viewArgs x = [x]

ppexpr :: Expr -> String
ppexpr = render . ppr 0
