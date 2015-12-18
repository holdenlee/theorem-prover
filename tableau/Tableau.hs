{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module Tableau where

import Language.Haskell.TH
import qualified Data.Map as M
import Prop

import IOUtils



$inst

-- $([d|instance Functor Prop' where $deriv|])
--not allowed

{-
instance Functor Prop' where
  fmap f = \case
    Prop' x -> Prop' (f x)
    Implies x1 x2 -> Implies (fmap f x1) (fmap f x2)
    Iff x1 x2 -> Iff (fmap f x1) (fmap f x2)
    And x1 x2 -> And (fmap f x1) (fmap f x2)
    Not x1 -> Not (fmap f x1)
-}

type Prop = Prop' PAtom

data Workspace = Workspace {_assms :: M.Map Int Prop, _concl :: M.Map Int Prop}
