{-# OPTIONS
 
 -XFlexibleInstances

#-}

module PropLogic where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Except
import Data.List
import Control.Monad.Free

import Utilities
import LeafTree
import Var
import Parser
import PMatch

type PAtom = WithVar IVar Str

newtype Statement = Statement (LeafTree PAtom)

instance (Show Statement) where
  show (Statement (Pure p)) = show p
  show (Statement (Free [x,y,z])) = "("++(intercalate " " $ map (show . Statement) [y,x,z])++")"

parseAtom :: Parser PAtom
parseAtom = (fmap (Var . IVar . read) (char '?' >> genWord)) <|> (fmap (JustA . Str) genWord)

parsePL = parseExpr parseAtom

--Test
pat1 = parse parsePL "" "(-> ?1 ?2)"
pat2 = parse parsePL "" "?1"
p1 = parse parsePL "" "(-> (-> P Q) R)"
p2 = parse parsePL "" "(-> P Q)"

test1 = do
  pat1' <- pat1
  p1' <- p1
  return $ pmatch [pat1'] [p1']

test :: Either ParseError (Maybe (M.Map IVar (LeafTree PAtom)))
test = do
  pat1' <- pat1
  pat2' <- pat2
  p1' <- p1
  p2' <- p2
  return (pmatch [pat1', pat2'] [p1', p2'])

