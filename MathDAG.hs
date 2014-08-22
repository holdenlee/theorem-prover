{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module MathDAG ( HowConclude, BoxContext (vars, assms), TreeIndex, FormulaInfo (howConclude, fType, treeIndex), emptyInfo, MathDAG, props, contextTree, formulaContext, formulaInfo, getProp, getFormulaInfo, getHowConclude, getTreeIndex, getFType, setFormulaInfo, setHowConclude, setTreeIndex, setFType, showProp, isOccupied, inmostTreeIndex, insertProp, insertProps, insertEdges, getByTreeIndex, setTreeIndices, addAt, addTreeIndex, setContextTree, initMathDAG, show2, showMathDAG) where
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

import Utilities
import Runner
import Type
--import TreeParser

--change later?

--data FormulaInfo = FormulaInfo {howConclude :: String
--                 , treeIndex :: TreeIndex}

--make this more sophisticated later
type HowConclude = String

data BoxContext = BoxContext {vars:: [Node]
		, assms :: [Node]}

type TreeIndex = [Int]

data FormulaInfo = FormulaInfo {howConclude :: HowConclude
                 , fType:: Type
                 , treeIndex :: TreeIndex}

instance Pointed FormulaInfo where
  point = FormulaInfo "" (Node "" []) []
  
emptyInfo::FormulaInfo
emptyInfo = point

data MathDAG = MathDAG { props:: Gr Formula Int
-- the base dependencies of each node.
             , contextTree :: Tree BoxContext
             , formulaContext :: MM.MultiMap Node Node
-- how each formula is concluded (a history)
             , formulaInfo :: Map.Map Node FormulaInfo
             --ignore this for now
             --, assmsGraph :: Gr Node ()
             } 

instance Pointed MathDAG where
  point = MathDAG Data.Graph.Inductive.empty (Node (BoxContext [] []) []) MM.empty Map.empty

-------------start getter functions

--Getters for props
getProp :: Node -> MathDAG -> Formula
getProp n md = 
  case (lab (props md) n) of
    Just form -> form
  --ignore Nothing case.

--Getters for formulaInfo

getFormulaInfo :: Node -> MathDAG -> FormulaInfo
getFormulaInfo n md = lookup2 n (formulaInfo md)

getHowConclude :: Node -> MathDAG -> HowConclude
getHowConclude n md = howConclude (getFormulaInfo n  md)

getTreeIndex ::Node -> MathDAG -> TreeIndex
getTreeIndex n md = treeIndex (getFormulaInfo n md)

getFType ::Node -> MathDAG -> Type
getFType n md = fType (getFormulaInfo n md)

getByTreeIndex:: (Searchable a b) => TreeIndex -> a -> b
getByTreeIndex ti tr = 
  case ti of 
    [] -> root tr
    (hd:rest) -> 
      let 
        cs = children tr
      in
        getByTreeIndex rest (cs !! hd)

setFormulaInfo :: Node -> FormulaInfo -> MathDAG -> MathDAG
setFormulaInfo n fi md = 
  let 
    fi = (getFormulaInfo n md)
  in
    md{formulaInfo = Map.insert n fi (formulaInfo md)}

setHowConclude :: Node -> HowConclude -> MathDAG -> MathDAG
setHowConclude n hc md = 
  let 
    fi = (getFormulaInfo n md)
  in
    md{formulaInfo = Map.insert n fi{howConclude = hc} (formulaInfo md)}

setTreeIndex ::Node -> TreeIndex -> MathDAG -> MathDAG
setTreeIndex n ti md = 
  let 
    fi = (getFormulaInfo n md)
  in
    md{formulaInfo = Map.insert n fi{treeIndex = ti} (formulaInfo md)}

setFType ::Node -> Type -> MathDAG -> MathDAG
setFType n ty md = 
  let 
    fi = (getFormulaInfo n md)
  in
    md{formulaInfo = Map.insert n fi{fType = ty} (formulaInfo md)}

setContextTree:: Tree BoxContext -> MathDAG ->MathDAG
setContextTree t m = m{contextTree = t}

show2 :: SymbolLib -> Formula -> String
show2 slib f = 
  let 
  --get the symbol at the root 
    rt = root f
  --look up the displayrule,
  --if you can't find it, use the default provided
    def = 
      if (null (children f)) then rt else (rt ++ "(" ++ "?args" ++ ")")
    drule = tryWithDefault (\sym -> do {
      x <- Map.lookup sym slib;
      return (displayRule x)}) def rt
  in (case (parseDispExpr drule (fmap (show2 slib) (children f))) of
    Right s -> s
    Left _ -> "error")

showProp :: SymbolLib -> Node -> MathDAG -> String
showProp slib n md = 
  let 
    prop = getProp n md
  in 
    (show n) ++ ": " ++ (show2 slib prop) ++ " (" ++ show (getTreeIndex n md) ++ ")"

showMathDAG :: SymbolLib -> MathDAG -> String
showMathDAG slib md = unlines (fmap (flip (showProp slib) md) (nodes (props md))) 

isOccupied:: Node -> MathDAG -> Bool
isOccupied n md = n `gelem` (props md)

inmostTreeIndex :: [TreeIndex] -> Maybe TreeIndex
inmostTreeIndex inds = 
  let 
    f xs ys = if (length xs >= length ys)
      then 
        if (take (length ys) xs == ys)
          then Just xs
        else Nothing
      else
        if (take (length xs) ys == xs)
          then Just ys
        else Nothing
  in 
    foldM f [] inds

--  -> TreeIndex
insertProp:: Node -> Formula -> MathDAG -> MathDAG
insertProp n prop md = 
  let
    p = props md
    fi = formulaInfo md
    p2 = insNode (n, prop) p
    fi2 = Map.insert n (emptyInfo) fi 
  in 
    md{props = p2, formulaInfo = fi2}
    
insertProps:: [Node] -> [Formula] -> MathDAG -> MathDAG
insertProps ns props md = foldIterate2 insertProp ns props md

setTreeIndices ::[Node] -> TreeIndex -> MathDAG -> MathDAG
setTreeIndices ns ti md = foldIterate (\n -> \m -> setTreeIndex n ti m) ns md

--insertProps:: [Node] -> Formula -> Tactic
--insertProps ns = joins 

insertEdges:: Node -> [Node]  -> MathDAG -> MathDAG
insertEdges goal hyps md = md{props = insEdges [(m, goal, i) | (m,i) <- (zip hyps [1..])] (props md)}

--adds at the next available location.
addAt:: (Searchable a b) => TreeIndex -> b -> a -> (a, TreeIndex)
addAt ti toAdd tr = 
  case ti of 
    [] -> (graft (root tr) (children tr ++ [node toAdd]), [length (children tr) + 1])
    (hd:rest) -> 
      let 
        cs = children tr
        (child, childTI) = addAt rest toAdd (cs!!hd)
      in
        (graft (root tr) ((take hd cs)++[child]++(drop (hd + 1) cs)), hd:childTI )

addTreeIndex ::  TreeIndex -> [Node] -> [Node] -> Tree BoxContext -> (Tree BoxContext, TreeIndex)
addTreeIndex  ti vars assms tr = 
--add the nodes
  let
    ctxt = BoxContext vars assms
  in
    addAt ti ctxt tr --add at the tree index, the context into tree.

initMathDAG :: Formula -> MathDAG
initMathDAG f = 
    point -: insertProp 1 f
	-: setTreeIndex 1 []


