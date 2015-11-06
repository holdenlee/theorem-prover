{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

{-# LANGUAGE TemplateHaskell #-}

module Workspace where

import Control.Monad
import Data.Graph.Inductive
import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM

import Control.Lens hiding (Context)

import Utilities
import Pointed

data BoxContext = BoxContext {_assms :: [Node]}

type TreeIndex = [Int]

data FormulaInfo a = FormulaInfo {_howConclude :: a
                   , _treeIndex :: TreeIndex}
  
data DAGWorkspace form ctxt hc = DAGWorkspace { _props:: Gr form Int
-- the graph shows the base dependencies of each node.
                                              , _contextTree :: Tree ctxt
-- keep track of nested environments
                                              , _formulaInfo :: M.Map Node (FormulaInfo hc)
-- map each node to information about the node.
                                              , _allKnown :: [Node]
                                              , _currentKnown :: [Node]
                                              , _currentGoals :: [Node]
                                              , _focus :: TreeIndex
                                              }

data WContext form = WContext {_library :: M.Map String form}
                             -- _symbolLib :: SymbolLib
                    --see old Type.hs

instance (Pointed a) => Pointed (FormulaInfo a) where
  point = FormulaInfo {_howConclude = point, _treeIndex = point}

instance (Pointed BoxContext) where
  point = BoxContext point

--figure out how to do this with template haskell, etc.!
instance (Pointed ctxt, Pointed hc) => Pointed (DAGWorkspace form ctxt hc) where
  point = DAGWorkspace {_props = point, _contextTree = point, _formulaInfo = point, _allKnown = point, _currentKnown = point, _currentGoals = point, _focus = point}

instance (Pointed form) => (Pointed (WContext form)) where
  point = WContext {_library = point}

makeLenses ''DAGWorkspace

