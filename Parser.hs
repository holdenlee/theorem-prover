module Parser where

import Text.Parsec
import Text.Parsec.String
import System.Environment
import LeafTree

--generalized word
genWord = many1 (noneOf " (),\n")

parseExpr :: Parser a -> Parser (LeafTree a)
parseExpr p = (spaces >> (p >>= (return . leaf))) <|>
  do {
    char '(';
    trees <- sepEndBy (parseExpr p) spaces;
    char ')';
    return $ node trees;
  }

parseLISP :: Parser (LeafTree String)
parseLISP = parseExpr genWord

{-
main = do
  s <- getArgs
  parseTest parseLISP (s!!0)
-}
