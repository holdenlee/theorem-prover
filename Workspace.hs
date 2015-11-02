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

import Control.Lens

import Utilities

data BoxContext = BoxContext {assms :: [Node]}

type TreeIndex = [Int]

data FormulaInfo a = FormulaInfo {_howConclude :: a
                   , _treeIndex :: TreeIndex}
  
data DAGWorkspace form ctxt hc = DAGWorkspace { _props:: Gr form Int
-- the graph shows the base dependencies of each node.
                                              , _contextTree :: Tree ctxt
-- keep track of nested environments
                                              , _formulaInfo :: M.Map Node (FormulaInfo hc)
-- map each node to information about the node.
                                              } 

makeLenses ''DAGWorkspace
