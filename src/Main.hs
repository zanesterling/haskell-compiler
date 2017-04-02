module Main where

import Control.Monad.Trans
import System.Console.Haskeline

import Check
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
    Right expr ->
      case checkTop [] expr of
        Left err -> print err
        Right t  -> do
          putStrLn $ ppexpr t
          -- Evaluate the expression
          let (v, steps) = runEval expr
          --mapM_ showStep steps
          putStrLn $ ppexpr v

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn $ (replicate d ' ') ++ "=> " ++ ppexpr x
