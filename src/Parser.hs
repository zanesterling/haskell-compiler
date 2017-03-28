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
lexer :: Tok.TokenParser ()
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


-- Prefix operators
boolOpsTable :: Ex.OperatorTable String () Identity BoolExpr
boolOpsTable = [
    [
      prefixOp "iszero" IsZero
    ]
  ]

numOpsTable  :: Ex.OperatorTable String () Identity NumExpr
numOpsTable  = [
    [
      prefixOp "succ"   Succ
    , prefixOp "pred"   Pred
    ]
  ]


-- if/then/else
ift :: Parser a -> (BoolExpr -> a -> a -> a) -> Parser a
ift e ifc = do
  reserved "if"
  cond <- boolExpr
  reservedOp "then"
  tr <- e
  reservedOp "else"
  fl <- e
  return $ ifc cond tr fl

boolIfThen :: Parser BoolExpr
boolIfThen = ift boolExpr BoolIf
numIfThen  = ift numExpr  NumIf


-- Constants
true, false :: Parser BoolExpr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl

zero :: Parser NumExpr
zero  = reservedOp "0"   >> return Zero

-- Expressions
expr :: Parser Expr
expr = (BoolExpr <$> boolExpr) <|> (NumExpr <$> numExpr)

boolExpr :: Parser BoolExpr
boolExpr = Ex.buildExpressionParser table boolFactor

boolFactor :: Parser BoolExpr
boolFactor =
      true
  <|> false
  <|> boolIfThen
  <|> parens boolExpr

numExpr :: Parser NumExpr
numExpr = Ex.buildExpressionParser table numFactor

numFactor :: Parser NumExpr
numFactor =
      zero
  <|> numIfThen
  <|> parens numExpr


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr s = parse (contents expr) "<stdin>" s
