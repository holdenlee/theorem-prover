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
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Tree
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

data WContext form = WContext {_library :: M.Map String form}
                             -- _symbolLib :: SymbolLib
                    --see old Type.hs

instance (Pointed BoxContext) where
  point = BoxContext point

--figure out how to do this with template haskell, etc.!
instance (Pointed ctxt, Pointed hc) => Pointed (DAGWorkspace form ctxt hc) where
  point = DAGWorkspace {_props = point, _contextTree = point, _treeIndices = point, _howConclude = point, _allKnown = point, _currentKnown = point, _currentGoals = point, _focus = point}

instance (Pointed form) => (Pointed (WContext form)) where
  point = WContext {_library = point}

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

--check to see if in already
--Left if it's forward reasoning, Right if its backwards
insertProp :: Either [G.Node] [G.Node] -> Bool -> hc -> form -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
insertProp e destr hc form w = 
    do
      let ins = fromMaybe [] $ getLeft e
      let outs = fromMaybe [] $ getRight e
      let n = (G.newNodes 1 (w ^. props))!!0
      let f = sequence . map (\x -> M.lookup x (w ^. treeIndices))
      ins' <- f ins
      outs' <- f outs
      deepest <- findRecord (ins' ++ outs')
      --the prop is now known if we're reasoning forwards, and all its predecessors were known
      let isNowKnown = (isLeft e) && all (`S.member` (w ^. allKnown)) ins 
        --add the node
      let w' = w & props %~ ((G.&) (zenumerate ins, n, form, zenumerate outs))
             --tag it with the deepest tree index in the premises or conclusion
                 & (treeIndices . at n) .~ (Just deepest)
                 & (howConclude . at n) .~ (Just hc)
             --whether to remove from goals or hyps
                 & (--if forward reasoning
                    if isLeft e
                    --if it is now known
                    then if isNowKnown  
                         then (currentKnown %~ S.insert n) .
                              (allKnown %~ S.insert n)
                         else id
                    else (currentGoals %~ (S.insert n . S.deletes outs)))
                 & (if destr && isLeft e
                    then currentKnown %~ S.deletes ins
                    else id)
      return (n, w')

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

connectProp :: Either [G.Node] [G.Node] -> Bool -> hc -> G.Node -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
connectProp e destr hc n w = 
    do
      let ins = fromMaybe [] $ getLeft e
      let outs = fromMaybe [] $ getRight e
      let f = sequence . map (\x -> M.lookup x (w ^. treeIndices))
      ins' <- f ins
      outs' <- f outs
      n' <- M.lookup n (w ^. treeIndices)
      deepest <- findRecord (ins' ++ outs' ++ [n'])
      --we have to make sure the node itself is compatible
      guard (n'==deepest)
      --the prop is now known if we're reasoning forwards, and all its predecessors were known, or it's known to begin with
      let isNowKnown = (n `S.member` (w ^. allKnown)) || ((isLeft e) && all (`S.member` (w ^. allKnown)) ins) 
        --add the node
      let w' = if isLeft e
               --if adding it as a conclusion, add incoming edges
               then w & (props . insLens n) .~ (zenumerate ins)
               --add how we concluded it
                      & (howConclude . at n) .~ Just hc
               --if it's now known, add to known nodes
                      & (if isNowKnown
                         then (currentKnown %~ S.insert n) .
                              (allKnown %~ S.insert n)
                         else id)
               --if destr, then delete the hypotheses (we're done with them)
                      & (if destr
                         then currentKnown %~ S.deletes ins
                         else id)
               --if adding it as a goal, add outgoing edges
               else w & (props . outsLens n) .~ (map (0,) outs) --how should we enumerate these?
               --if it's already known, don't do anything, else it's a new goal
                      & (if isNowKnown 
                         then id
                         else currentGoals %~ S.insert n)
      return (n, w')

--(r -> s -> [((a, w), s)]), s is the workspace
{-| f is a function that given a context and a list of formulas, will attempt to
* build a formula out of it, 
* give a internal representation of how it concluded (hc)
* give a human-readable representation of what it did (str)
|-}
forwardReason' :: (Eq form) => (ctxt -> [form] -> Maybe (form, hc, str)) -> Bool -> [G.Node] -> ctxt -> DAGWorkspace form ctxt hc -> [((G.Node, str), DAGWorkspace form ctxt hc)]
forwardReason' f destr nds ctxt w = maybeToList $ 
    do 
      --get hyps by looking up the nodes in the proposition graphs
      let hyps = map (\x -> w ^. (props . nodeLens x)) nds 
      --now apply f on them
      (concl, hc, str) <- f ctxt hyps
      let maybeN = findFirstIndexOf (==concl) $ G.labNodes (w ^. props)
      --does the conclusion already exists in the graph?
      (case maybeN of
--connectProp :: Either [G.Node] [G.Node] -> Bool -> hc -> G.Node -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
        --connect up the props 
        Just n -> w & (connectProp (Left nds) destr hc n) 
--insertProp :: Either [G.Node] [G.Node] -> Bool -> hc -> form -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
        Nothing -> (w & (insertProp (Left nds) destr hc concl))) 
                --add the message
                |> fmap (first (,str))
--WARNING: the conclusion treeindex must be correct!
--connectProp fails if not deepest treeidex, use to advantage?
--TODO

--is there a formula on the workspace that makes the same assumptions?
findFormula :: (Eq form) => TreeIndex -> form -> DAGWorkspace form ctxt hc -> Maybe G.Node
findFormula ti form w = fmap fst $ listToMaybe $ filter (\(n, l) -> l==form && ((w ^. treeIndices) M.! n) == ti) $ G.labNodes (w ^. props) 
--or less
--(isInitialSegment ((w ^. treeIndices) M.! n) ti)

{-
addOrFind :: TreeIndex -> form -> DAGWorkspace form ctxt hc -> G.Node
addOrFind ti form w = 
    let
        maybeN = findFirstIndexOf (==form) $ G.labNodes (w ^. props)
    in
      case maybeN of
        Just n -> 
            case w -}
{-
backwardReason' :: (Eq form) => (ctxt -> form -> Maybe ([form], hc, str)) -> Bool -> [G.Node] -> ctxt -> DAGWorkspace form ctxt hc -> [((G.Node, str), DAGWorkspace form ctxt hc)]
backwardReason' f destr nds ctxt w = maybeToList $ 
    do 
      --get hyps by looking up the nodes in the proposition graphs
      let hyps = map (\x -> w ^. (props . nodeLens x)) nds 
      --now apply f on them
      (concl, hc, str) <- f ctxt hyps
      let maybeN = findFirstIndexOf (==concl) $ G.labNodes (w ^. props)
      --does the conclusion already exists in the graph?
      (case maybeN of
--connectProp :: Either [G.Node] [G.Node] -> Bool -> hc -> G.Node -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
        --connect up the props 
        Just n -> w & (connectProp (Left nds) destr hc n) 
--insertProp :: Either [G.Node] [G.Node] -> Bool -> hc -> form -> DAGWorkspace form ctxt hc -> Maybe (G.Node, DAGWorkspace form ctxt hc)
        Nothing -> (w & (insertProp (Left nds) destr hc concl))) 
                --add the message
                |> fmap (first (,str))
-}
