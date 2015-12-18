{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module Prop where

import Language.Haskell.TH
import Control.Monad
import TemplateUtils

type PName = String

data PAtom = PName PName | PVar Int

data Prop' p = Prop' p | Implies (Prop' p) (Prop' p) | Iff (Prop' p) (Prop' p) | And (Prop' p) (Prop' p) | Or (Prop' p) (Prop' p) | Not (Prop' p) deriving Show

type Prop = Prop' PAtom

deriveFmap ''Prop'
--this automatically generates the following.
{-
instance Functor Prop' where
  fmap f = \case
    Prop' x -> Prop' (f x)
    Implies x1 x2 -> Implies (fmap f x1) (fmap f x2)
    Iff x1 x2 -> Iff (fmap f x1) (fmap f x2)
    And x1 x2 -> And (fmap f x1) (fmap f x2)
    Not x1 -> Not (fmap f x1)
-}
