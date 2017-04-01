module Parser (
  parseExpr
) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Lexer
import Syntax


-- Expressions
expr :: Parser Expr
expr = foldl1 App <$> many1 term

term :: Parser Expr
term =
      lambda
  <|> literal
  <|> variable
  <|> parens expr


lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  arg <- identifier
  reservedOp ":"
  argtype <- argtype
  reservedOp "."
  body <- expr
  return $ Lam arg argtype body

literal :: Parser Expr
literal = number <|> bool

number :: Parser Expr
number = Lit . LInt  . fromIntegral <$> natural

bool :: Parser Expr
bool = Lit . LBool . read <$> (symbol "True" <|> symbol "False")

variable :: Parser Expr
variable = Var <$> identifier


-- Types
argtype :: Parser Type
argtype = Ex.buildExpressionParser typeOps typeAtom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    typeOps = [
        [infixOp "->" TArr Ex.AssocRight]
      ]

typeAtom :: Parser Type
typeAtom = typeLiteral <|> parens argtype

typeLiteral :: Parser Type
typeLiteral =
      (reservedOp "Bool" >> return TBool)
  <|> (reservedOp "Int"  >> return TInt)


-- Interface
parseExpr s = parse (contents expr) "<stdin>" s
