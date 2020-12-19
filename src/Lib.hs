{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.Except
import Data.Char
import qualified Data.HashMap as M
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Pretty.Simple (pPrint)

data Command
  = Next
  | Prev
  | Inc
  | Dec
  | Print
  | Input
  | Loop [Command]
  deriving (Show)

type Parser = Parsec Void T.Text

type Cells = M.Map Index Int

type Index = Int

cells :: Cells
cells = M.empty

binop :: Index -> Cells -> (Int -> Int) -> Cells
binop index cells f = case M.lookup index cells of
  Nothing -> M.insert index (f 0) cells
  Just v -> M.insert index (mod (abs $ f v) 256) cells

lookupCells :: Index -> Cells -> Int
lookupCells index cells = case M.lookup index cells of
  (Just v) -> v
  Nothing -> 0

eval :: [Command] -> Index -> Cells -> IO (Index, Cells)
eval [] index cells = return (index, cells)
eval (Inc : rest) index cells = eval rest index (binop index cells (+ 1))
eval (Dec : rest) index cells = eval rest index (binop index cells (subtract 1))
eval (Next : rest) index cells = eval rest (index + 1) cells
eval (Prev : rest) index cells = eval rest (index - 1) cells
eval l@(Loop content : rest) index cells =
  if lookupCells index cells == 0
    then eval rest index cells
    else do
      (ind, cls) <- eval content index cells
      eval l ind cls
eval (Print : rest) index cells = putStr (init . tail . show . chr $ lookupCells index cells) >> eval rest index cells
eval (Input : rest) index cells = do
  v <- getChar
  eval rest index $ M.insert index (ord v) cells

pb :: Char -> Command -> Parser Command
pb c v = single c >> return v

nextParser :: Parser Command
nextParser = pb '>' Next

prevParser :: Parser Command
prevParser = pb '<' Prev

incParser :: Parser Command
incParser = pb '+' Inc

decParser :: Parser Command
decParser = pb '-' Dec

printParser :: Parser Command
printParser = pb '.' Print

inputParser :: Parser Command
inputParser = pb ',' Input

parseExpr :: Parser [Command]
parseExpr = many (space >> nextParser <|> prevParser <|> incParser <|> decParser <|> printParser <|> inputParser <|> loopParser)

loopParser :: Parser Command
loopParser = do
  single '['
  content <- parseExpr
  single ']'
  return $ Loop content

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseAndEval :: T.Text -> IO ()
parseAndEval input = case parse parseExpr "brainfuck" input of
  Right v -> do
    res <- eval v 0 cells
    putStrLn ""
  Left err -> pPrint err