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

import NTactic

type Search a = NProofState (a -> [a]) [a] a

--nProofState :: (Monoid l) => (c -> [(w, l)]) -> NProofState c l w
step :: a -> Search a
step x = nProofState (\f -> map (\y -> (y, [y])) $ f x)

restr :: String -> Bool
restr s = all (\l -> length (elemIndices l s) <= 2) "abcdefghijk"

f x = filter restr $ map (x++) ["a","b","c","d","e","f","g","h","i","j","k"]

--eval :: NProofState c l w -> c -> (w, l)
test = eval ("" & (repeatT 17 step)) f
               --(step .& step .& step .& step .& step))

