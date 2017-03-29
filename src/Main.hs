module Main where

import Control.Monad.Trans
import System.Console.Haskeline

import Parser


main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Untyped> "
    case minput of
      Nothing  -> outputStrLn "Goodbye."
      Just val -> (liftIO $ process val) >> loop

process :: String -> IO ()
process line =
  case parseExpr line of
    Left err -> print err
    Right exp -> print exp
