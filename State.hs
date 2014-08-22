{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module State (State, get, set, modify) where
import System.Environment
import Control.Monad
import qualified Data.Graph.Inductive as G
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Search
import MathParser
import qualified Data.Hashable
import Data.Maybe
import Data.Dynamic


import Utilities
import Runner

type State = Map.Map String Dynamic

get :: (Pointed c, Typeable c) => String -> State -> c
get str state = tryWithDefault (\x -> do {
  y <- Map.lookup x state;
  fromDynamic y}) point str
  
set :: (Typeable a) => String -> a -> State -> State
set str x state = Map.insert str (toDyn x) state

modify :: (Typeable a, Pointed a) => String -> (a->a) -> State -> State
modify str f state = Map.adjust (\x -> toDyn $ f $ fromDyn  x point) str state
