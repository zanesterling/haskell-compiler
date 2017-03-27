module Main where

import Control.Monad.Trans
import System.Console.Haskeline

import Syntax
import Parser
import Pretty

process :: String -> IO ()
process line =
  case parseExpr line of
    Left err -> print err
    Right ex -> putStrLn $
      case eval ex of
        Nothing  -> "Cannot evaluate."
        Just val -> ppexpr val

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Repl> "
    case minput of
      Nothing  -> outputStrLn "Goodbye."
      Just val -> (liftIO $ process val) >> loop
