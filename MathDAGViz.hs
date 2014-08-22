{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module MathDAGViz (mathDAGToDot,mathSessionToDot, mathDAGToDotC, mathSessionToDotC) where
import System.Environment
import Control.Monad
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import Search
import MathParser
import qualified Data.Hashable
import Data.Monoid
import qualified Data.MultiMap as MM
import Data.Dynamic
import Data.Text.Lazy (Text, pack, unpack)

--graph visualization
import Data.GraphViz
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete

import Utilities
import MathDAG
import MathSession
import Type
import DAGViz
{-
 http://hackage.haskell.org/package/graphviz-2999.17.0.1/docs/Data-GraphViz.html

-}

--libToParams :: SymbolLib -> Gr Formula Int -> GraphvizParams Node Formula Int () Formula
--libToParams slib graph = makeParams (\n form g -> showProp slib n g) graph

mathDAGToDot :: SymbolLib -> MathDAG -> String
mathDAGToDot slib md = defaultDot (\n form g -> showProp slib n md) (props md)

mathSessionToDot :: MathSession -> String
mathSessionToDot ms = mathDAGToDot (symbolLib ms) (mathDAG ms)

-- | with clustering
mathDAGToDotC :: SymbolLib -> MathDAG -> String
mathDAGToDotC slib md = defaultDotC (\n form -> showPropOnly slib n md) (\n -> getTreeIndex n md) (props md)

mathSessionToDotC :: MathSession -> String
mathSessionToDotC ms = mathDAGToDotC (symbolLib ms) (mathDAG ms)
