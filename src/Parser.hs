module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

import Syntax

-- Tokens
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }


-- Lexer
lexer :: Tok.TokenParser()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)


-- Operators
table :: Ex.OperatorTable String () Identity Expr
table = [
  ]


expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = 
      lambda
  <|> parens application
  <|> variable
  <|> parens expr


lambda :: Parser Expr
lambda = do
  char '\\'
  argname <- varname
  char '.'
  e <- expr
  return $ Lam argname e

application :: Parser Expr
application = do
  x <- expr
  Tok.whiteSpace lexer
  xs <- many1 $ do { x <- expr; Tok.whiteSpace lexer; return x }
  return $ foldl (\a x -> App a x) x xs

variable :: Parser Expr
variable = Var <$> varname

varname :: Parser Name
varname = many1 alphaNum


-- Interface
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr s = parse (contents expr) "<stdin>" s
