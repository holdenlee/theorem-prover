{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

{-# OPTIONS
    -XTemplateHaskell
    -XDeriveDataTypeable
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XTypeSynonymInstances
    -XFlexibleInstances
#-}

module Prop where

import Language.Haskell.TH
import Control.Lens
import Control.Monad
import Data.Data
import Data.Typeable
import TemplateUtils

type PName = String

data PAtom = PName PName | PVar Int deriving (Show, Eq, Typeable, Data, Ord)

data Prop' p = Prop' p | Implies (Prop' p) (Prop' p) | Iff (Prop' p) (Prop' p) | And (Prop' p) (Prop' p) | Or (Prop' p) (Prop' p) | Not (Prop' p) deriving (Show, Eq, Typeable, Data, Ord)

type Prop = Prop' PAtom

deriveFunctor ''Prop'
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
deriveApplicative 'Prop' ''Prop'
deriveMonad ['Prop'] ''Prop'
deriveTraversable ''Prop'

data DeductionRule = DeductionRule {_deductionRuleAssms :: [Prop],
                                    _deductionRuleConcl :: Prop,
                                    _deductionRuleArgs :: Int,
                                    _deductionRuleName :: String
                                   } deriving (Show, Eq, Typeable, Data, Ord)

makeFields ''DeductionRule
