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

import Tactic

type Search a = Tactic (a -> [a]) [a] a

step :: a -> Search a
step x = do
  f <- ask
  WriterT $ ListT $ return $ map (\y -> (y, [y])) $ f x

restr :: String -> Bool
restr s = all (\l -> length (elemIndices l s) <= 2) "abcd"

f x = filter restr $ map (x++) ["a","b","c","d"]

eval :: (a -> [a]) -> Search a -> (a, [a])
eval g s = (g & (runReader $ runListT $ runWriterT s))!!0

test = eval f ("" & (step .& step .& step .& step .& step))

