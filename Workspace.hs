{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XTupleSections
#-}

{-# LANGUAGE TemplateHaskell #-}

module Workspace where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Tree
import Data.Tree.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Number.PartialOrd
import Data.Bifunctor
import Data.Either
import Data.Maybe

import Control.Lens hiding (Context, (|>))

import Utilities
import SetUtilities as S
import Pointed
import GraphUtils

data BoxContext = BoxContext {_assms :: [G.Node]}

type TreeIndex = [Int]

{-
data FormulaInfo a = FormulaInfo {_howConclude :: a
                   , _treeIndex :: TreeIndex}-}

data DAGWorkspace form ctxt hc = DAGWorkspace { _props:: G.Gr form Int
-- the graph shows the base dependencies of each node.
                                              , _contextTree :: Tree ctxt
-- keep track of nested environments
                                              , _treeIndices :: M.Map G.Node TreeIndex
                                              , _howConclude :: M.Map G.Node hc
-- map each node to information about the node.
                                              , _allKnown :: S.Set G.Node
                                              , _currentKnown :: S.Set G.Node
                                              , _currentGoals :: S.Set G.Node
                                              , _focus :: TreeIndex
                                              }

lookupTI :: TreeIndex -> Tree ctxt -> ctxt
lookupTI ti tree =
  let children = subForest tree
  in
   case ti of
    [] -> rootLabel tree
    h:rest -> lookupTI rest (children!!h)

insertTI :: ctxt -> TreeIndex -> Tree ctxt -> (Int, Tree ctxt)
insertTI ctxt ti tree =
  let children = subForest tree
  in
   case ti of
    [] -> ((length children) + 1, tree & branches %~ (++[Node ctxt []]))
    h:rest ->
      let
        (n, t) = insertTI ctxt rest (children!!h)
      in
       (n, tree & (branches . ix h) .~ t)

--data WContext form = WContext {_library :: M.Map String form}
                             -- _symbolLib :: SymbolLib
                    --see old Type.hs

instance (Pointed BoxContext) where
  point = BoxContext point

--figure out how to do this with template haskell, etc.!
instance (Pointed ctxt, Pointed hc) => Pointed (DAGWorkspace form ctxt hc) where
  point = DAGWorkspace {_props = point, _contextTree = point, _treeIndices = point, _howConclude = point, _allKnown = point, _currentKnown = point, _currentGoals = point, _focus = point}

{-instance (Pointed form) => (Pointed (WContext form)) where
  point = WContext {_library = point}-}

makeLenses ''DAGWorkspace

findFirstIndexOf :: (b -> Bool) -> [(a,b)] -> Maybe a
findFirstIndexOf filt = fmap fst . mlookup 0 . filter (filt . snd)

instance PartialOrd TreeIndex where
    cmp ti1 ti2 = case (stripPrefix ti1 ti2, stripPrefix ti2 ti1) of 
                    (Just [], _) -> Just EQ
                    (Just _, _) -> Just LT
                    (Nothing, Just _) -> Just GT
                    _ -> Nothing

--pointed is minimum
findRecord :: (PartialOrd a, Pointed a) => [a] -> Maybe a
findRecord = foldl (\x y -> x >>= (\x' -> do
                      case cmp x' y of
                        Nothing -> Nothing
                        Just LT -> Just y
                        _ -> Just x')) (Just point)

wNewNode :: DAGWorkspace form ctxt hc -> G.Node
wNewNode w = (G.newNodes 0 (w ^. props))!!0

insProp :: TreeIndex -> hc -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
insProp ti hc form w = 
    let 
        n = wNewNode w
    in 
      (n, w & props %~ (G.insNode (n, form))
            & (treeIndices . at n) .~ Just ti
            & (howConclude . at n) .~ Just hc)

checkTI :: [G.Node] -> DAGWorkspace form ctxt hc -> Maybe TreeIndex
checkTI nds w = 
    do
      tis <- sequence $ map (\x -> M.lookup x (w ^. treeIndices)) $ nds
      findRecord tis

findFormula :: (Eq form) => (TreeIndex -> Bool) -> form -> DAGWorkspace form ctxt hc -> Maybe G.Node
findFormula tif form w = fmap fst $ listToMaybe $ filter (\(n, l) -> l==form && tif ((w ^. treeIndices) M.! n)) $ G.labNodes (w ^. props)

findOrInsProp :: (Eq form) => (TreeIndex -> Bool) -> TreeIndex -> hc -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
findOrInsProp tif ti hc form w = 
    case findFormula tif form w of
--findFormula :: (Eq form) => TreeIndex -> form -> DAGWorkspace form ctxt hc -> Maybe G.Node
        Just n -> (n, w)
        Nothing -> insProp ti hc form w

connectForwardProp :: [G.Node] -> G.Node -> Bool -> DAGWorkspace form ctxt hc -> DAGWorkspace form ctxt hc
connectForwardProp ins n destr w =
    let isNowKnown = all (`S.member` (w ^. allKnown)) ins 
    in
      w & (props . insLens n) .~ (zenumerate ins)
        & (if destr 
           then currentKnown %~ S.deletes ins
           else id)

--right now only connect to things ==ti, rather than >=ti
forwardProp :: (Eq form) => [G.Node] -> TreeIndex  -> Bool -> hc -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
forwardProp ins ti destr hc form w = 
    (\(n, w') -> (n, connectForwardProp ins n destr w')) $ findOrInsProp (==ti) ti hc form w
        
--only spread goaliness to new nodes?
backwardProp :: (Eq form) => G.Node -> TreeIndex -> hc -> [form] -> DAGWorkspace form ctxt hc -> ([G.Node], DAGWorkspace form ctxt hc)
backwardProp out ti hc forms =
  runState $ do
    w <- get
--    let ti = fromJust (w ^. treeIndices . at out)
    --(TreeIndex -> Bool) -> TreeIndex -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
    nds <- sequence $ map (\f -> state (\w' -> findOrInsProp ((==Just True) . (`le` ti)) ti hc f w')) forms
    return nds

propagateKnowns' :: [G.Node] -> DAGWorkspace form ctxt hc -> DAGWorkspace form ctxt hc
propagateKnowns' nds w = 
    let
        knowns = w ^. allKnown
        (w2, remNds) = foldl (\(w', li) n -> if (all (`S.member` knowns) $ map snd (w' ^. (props . insLens n)))
                                             --if all the predecessors are known, it is now known (note I'm not adding it to curKnowns)
                                             then (w' & allKnown %~ S.insert n, li)
                                             --else, n is still not known.
                                             else (w', n:li)) (w, []) nds
    in
      if length nds == length remNds
      --made no progress
      then w2
      else propagateKnowns' remNds w2

propagateKnowns w = propagateKnowns' (filter (\x -> not $ x `S.member` (w ^. allKnown)) $ G.nodes (w ^. props)) w

--(r -> s -> [((a, w), s)]), s is the workspace
{-| f is a function that given a context and a list of formulas, will attempt to
* build a formula out of it, 
* give a internal representation of how it concluded (hc)
* give a human-readable representation of what it did (str)
|-}
forwardReason' :: (Eq form) => (c -> [form] -> Maybe (form, hc, str)) -> Bool -> [G.Node] -> c -> DAGWorkspace form ctxt hc -> [((G.Node, str), DAGWorkspace form ctxt hc)]
forwardReason' f destr ins ctxt w = maybeToList $ 
    do 
      --get hyps by looking up the nodes in the proposition graphs
      let hyps = map (\x -> w ^. (props . nodeLens x)) ins 
      --now apply f on them
      (concl, hc, str) <- f ctxt hyps
      --checkTI :: [Node] -> DAGWorkspace form ctxt hc -> Maybe TreeIndex
      ti <- checkTI ins w
      -- [G.Node] -> TreeIndex  -> Bool -> hc -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
      let (n, w') = w & forwardProp ins ti destr hc concl
      return ((n, str), propagateKnowns w')

-- G.Node -> hc -> [form] -> DAGWorkspace form ctxt -> ([G.Node], DAGWorkspace form ctxt hc)
backwardReason' :: (Eq form) => (c -> form -> Maybe ([form], hc, str)) -> G.Node -> c -> DAGWorkspace form ctxt hc -> [(([G.Node], str), DAGWorkspace form ctxt hc)]
backwardReason' f out ctxt w = maybeToList $ 
    do 
      --get goal by looking up the node in the proposition graphs
      let goal = w ^. (props . nodeLens out) 
      --now apply f on it
      (hyps, hc, str) <- f ctxt goal
      let ti = fromJust $ w ^. (treeIndices . at out)
      -- G.Node -> hc -> [form] -> DAGWorkspace form ctxt hc -> ([G.Node], DAGWorkspace form ctxt hc)
      let (ns, w') = backwardProp out ti hc hyps w
      return ((ns, str), propagateKnowns w')

unfoldProp :: G.Node -> TreeIndex -> hc -> [form] -> form -> DAGWorkspace form ctxt hc -> (([G.Node], G.Node), DAGWorkspace form ctxt hc)
unfoldProp n ti hc assms goal =
  runState $ do
    w <- get
--insProp :: TreeIndex -> hc -> form -> DAGWorkspace form ctxt hc -> (G.Node, DAGWorkspace form ctxt hc)
    assmsNode <- sequence $ map (\f -> state (\w' -> insProp ti hc f w')) assms
    goalNode <- state (insProp ti hc goal)
    modify (((props . insLens n) .~ [(0, goalNode)]) .
            (howConclude . at n .~ Just hc))
    return (assmsNode, goalNode)

unfold' :: (Eq form) => (c -> form -> Maybe (([form], form), hc, str)) -> G.Node -> c-> DAGWorkspace form BoxContext hc -> [((([G.Node], G.Node, TreeIndex), str), DAGWorkspace form BoxContext hc)]
unfold' f n ctxt w = maybeToList $
    do
      --look up node in proposition graph
      let pr = w ^. (props . nodeLens n)
      --now apply f on it
      ((assm, goal), hc, str) <- f ctxt pr
      let newNs = G.newNodes (length assm) (w ^. props)
      let ti = fromJust $ w ^. (treeIndices . at n)
      let (i,contextTree') = insertTI (BoxContext {_assms = newNs}) ti (w ^. contextTree)
      let ti' = ti++[i]
      let w' = w & contextTree .~ contextTree'
      --G.Node -> TreeIndex -> hc -> [form] -> form -> DAGWorkspace form ctxt hc -> [(([G.Node], G.Node), DAGWorkspace form ctxt hc)]
      let ((assmsNode, goalNode), w'') = unfoldProp n ti' hc assm goal w'
      return (((assmsNode, goalNode, ti'), str), w'')

--should give the writer access to the nodes!
