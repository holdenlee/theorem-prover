{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XTupleSections
 -XUndecidableInstances
#-}

{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Nondet
import Control.Applicative

check :: (s -> Bool) -> Nondet s ()
check f = Nondet (\s -> if f s then [((), s)] else [])

dfs' :: (s -> [s]) -> (s -> Bool) -> Nondet s ()
dfs' children f = 
  (check f) <|> (Nondet (map ((),) . children) >> (dfs' children f))

dfs :: (s -> [s]) -> (s -> Bool) -> s -> s
dfs children f s = (!!0) $ execNondet (dfs' children f) s

test = dfs (\x -> filter (<=37) [2*x, 2*x+1]) (==37) 1

--this doesn't terminate:(
main = putStrLn $ show test

--bestFSEx = bestFSMem head (\path -> length path + l1 (10,10) (head path))

{-
--this terminates. so what goes wrong above?
import Control.Monad
let f x = (if x==42 then [x] else [])++((filter (<=42) [2*x, 2*x+1]) >>= f)
(f 1)!!0
-}
