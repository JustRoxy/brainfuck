module Main where

import qualified Data.Text as T
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> putStrLn "0 args found"
    _ -> parseAndEval (T.pack $ head args)
