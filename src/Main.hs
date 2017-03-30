module Main where

import Control.Monad.Trans
import System.Console.Haskeline

import Eval
import Parser
import Pretty
import Syntax


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
    Right exp -> do
      let (v, steps) = runEval exp
      mapM_ showStep steps
      print v

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn $ (replicate d ' ') ++ "=> " ++ ppexpr x
