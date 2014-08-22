{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XScopedTypeVariables
#-}

module DAGViz (makeParams, defaultVis, toDotString, defaultDot) where
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
--import MathDAG
{-
 http://hackage.haskell.org/package/graphviz-2999.17.0.1/docs/Data-GraphViz.html

-}

--Node Formula Int
makeParams :: (Show el) => (Node -> nl -> Gr nl el -> String) -> Gr nl el -> GraphvizParams Node nl el () nl
makeParams f graph = nonClusteredParams {
  --clusterBy = cb,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    fn (xn,xl) = [(Label . StrLabel. pack) (f xn xl graph)]
    fe (xm,xn,l) = [(Label . StrLabel. pack) (show l)]

defaultVis :: (Show el) => (Node -> nl  -> Gr nl el-> String) -> Gr nl el -> DotGraph Node
defaultVis f g = graphToDot (makeParams f g) g

toDotString :: DotGraph Node -> String
toDotString dg = unpack $ renderDot $ toDot dg

defaultDot :: (Show el) => (Node -> nl -> Gr nl el -> String) -> Gr nl el -> String
defaultDot f g = toDotString (defaultVis f g)

--same but with clustering

{-
input: a function from nodes & labels to a string,
a function from a node & label to a cluster label, and label of cluster parent.
-}
{-
makeClusterParams :: (Show el) => (Node -> nl -> String) -> (Node -> nl -> (cl,Maybe cl)) -> Gr nl el -> GraphvizParams Node nl el cl nl
makeClusterParams f g graph = nonClusteredParams {
  clusterBy = cb,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    cb (xn,xl) = 
      case g xn xl of 
	(thisCluster, Just parentCluster) -> --put this node inside of thisCluster, and put thisCluster inside of parentCluster
	(thisCluster, Nothing) -> --put this node in the top-level cluster.
    fn (xn,xl) = [(Label . StrLabel. pack) (f xn xl)]
    fe (xm,xn,l) = [(Label . StrLabel. pack) (show l)]
-}




