module Parser (
  parseExpr
) where

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

identifier :: Parser String
identifier = Tok.identifier lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

natural :: Parser Integer
natural = Tok.natural lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer


-- Operators
table :: Ex.OperatorTable String () Identity Expr
table = [
  ]


expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      try application
  <|> subExpr

subExpr :: Parser Expr
subExpr =
      lambda
  <|> literal
  <|> variable
  <|> parens expr


lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

application :: Parser Expr
application = do
  x <- subExpr
  whitespace
  xs <- many1 $ do { x <- subExpr; whitespace; return x }
  return $ foldl App x xs

variable :: Parser Expr
variable = Var <$> varname

literal :: Parser Expr
literal = number <|> bool

number :: Parser Expr
number = Lit . LInt  . fromIntegral <$> natural

bool :: Parser Expr
bool = Lit . LBool . read <$> (symbol "True" <|> symbol "False")

varname :: Parser Name
varname = many1 alphaNum


-- Interface
contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

parseExpr s = parse (contents expr) "<stdin>" s
