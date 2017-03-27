{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative


newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])


-- A Functor is a data structure that you can map over.
-- Calling fmap on a function and a Functor returns a Functor where each item
-- is mapped under the function.
instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [(f a, b) | (a, b) <- cs s]

-- An Applicative Functor is a Functor which allows you to
-- bind a `Monad (a -> b)` with a `Monad a` to produce a `Monad b`.
instance Applicative Parser where
  pure = unit
  (Parser cs1) <*> (Parser cs2) = Parser $ apply cs1 cs2
    where apply cs1 cs2 s = [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

-- A Monad is like an action to be performed and a context to perform it in.
-- Calling return on a value wraps the value in a Monad.
-- The bind (>>=) function composes two Monads together.
instance Monad Parser where
  return = unit
  (>>=)  = bind


-- Produce the empty parser.
failure :: Parser a
failure = Parser (\cs -> [])

-- Concat the results of parsing with each of two parsers.
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

-- Try parsing with the first parser, and if it fails use the second.
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option



-- | One or more.
-- some :: Alternative f => f a -> f [a]
-- some v = some_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- | Zero or more.
-- many :: Alternative f => f a -> f [a]
-- many v = many_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v


general_satisfy :: Parser a -> (a -> Bool) -> Parser a
general_satisfy parser pred = parser `bind` \a ->
  if pred a
  then unit a
  else failure

satisfy = general_satisfy item


oneOf :: [Char] -> Parser Char
oneOf s = satisfy $ flip elem s

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a ->a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
  where rest a = (do f <- op
                     b <- p
                     rest (f a b)
                 ) <|> return a


char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string []     = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n


-- Expression types, evaluation, and parsing.
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval ex = case ex of
  (Add a b) -> eval a + eval b
  (Mul a b) -> eval a * eval b
  (Sub a b) -> eval a - eval b
  (Lit n)   -> n

int :: Parser Expr
int = do
  n <- number
  return $ Lit n

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp s op = reserved s >> return op

addop = infixOp "+" Add
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr
