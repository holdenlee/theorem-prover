module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Free
import System.Environment
import Data.Tree

type LeafTree = Free [] 

leaf = Pure
node = Free

--generalized word
genWord = many1 (noneOf " (),\n")

parseLISP :: Parser (LeafTree String)
parseLISP = spaces >> (
  do {
    x <- genWord; --String
    return $ leaf x;
  } <|>
  do {
    char '(';
    trees <- sepEndBy parseLISP spaces;
    char ')';
    return $ node trees;
  })

main = do
  s <- getArgs
  parseTest parseLISP (s!!0)
