module Main where

import Control.Monad
import System.IO

import NanoParsec

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  a <- getLine
  print $ eval $ run a
