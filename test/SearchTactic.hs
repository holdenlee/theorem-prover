{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XFlexibleContexts
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XTupleSections
#-}

module SearchTactic where

import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.List
import Control.Lens

import Comb
import Tactic

type Search a = ProofState [a] (a -> [a]) a ()

--proofState :: (r -> s -> [((a, w), s)]) -> ProofState w r s a
step :: Search a
step = proofState (\f x -> map (\y -> (((), [y]), y)) $ f x)

restr :: String -> Bool
restr s = all (\l -> length (elemIndices l s) <= 2) "abcdefghijk"

f x = filter restr $ map (x++) ["a","b","c","d","e","f","g","h","i","j","k"]

--evalProofState :: (Monoid w) => ProofState w r s a -> r -> s -> (a, w)
test = evalProofState (sequence $ replicate 17 step) f ""

