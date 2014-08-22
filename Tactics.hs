{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Tactics ( Tactic, insertEdgesT, insertPropT, forwardReason, exportProp, unfoldProp) where
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
import MathDAG
import Pattern
import State

type Tactic = Runner MathDAG MathDAG State

insertEdgesT:: Node -> [Node]  -> Tactic
insertEdgesT n ns = rmap (insertEdges n ns)

insertPropT:: Node -> Formula -> Tactic
insertPropT n prop = rmap (insertProp n prop)

--should this be a tactic?
forwardReason:: Formula -> [Node] -> Node -> Tactic
forwardReason thm hyps goal = funToRunner (\(md, state, exit) ->
  let
    --goal = (freeNodes md) !! 0
    p = props md
    c = contextTree md
    fi = formulaInfo md
    --FIRST find treeIndex
    mti = inmostTreeIndex [treeIndex (lookup2 m fi) | m <- hyps]
    --only proceed if checkTreeIndex == ctxt2.
    --try to find a substitution of the given nodes into the hypothesis of f.
    (concl, sub, exit2) = Runner.run (findSubs [getProp n md | n <- hyps]) thm
  in case mti of 
   Nothing -> (md, state -: set "message" "Propositions live in different spaces", Fail)
   Just ti ->
    case exit2 of 
      Fail -> (md, state -: set "message" ("Failed to find substitution: " ++
		"Tried to substitute " ++ "\n" ++ unlines (fmap (\h -> showProp (Map.empty) h md) hyps) ++
		"\n" ++ "into theorem " ++ show2 Map.empty thm), Fail) --failed to find sub, return original mathdag.
      _  -> 
	if goal `gelem` p
	  then
           let goalTI = getTreeIndex goal md
           in
	    if concl == removeJust (lab p goal) && inmostTreeIndex [ti, goalTI] == Just goalTI
              then (md -: setHowConclude goal "forwardReason"
            	-: insertEdges goal hyps, 
		state -: set "message" "Proved proposition", OK)	
	      else (md, state -: set "message" "Does not equal given goal!", Fail)
          else (md -: insertProp goal concl 
            -: setTreeIndex goal ti
            -: setHowConclude goal "forwardReason"
            -: insertEdges goal hyps, state -: set "message" "Added proposition", OK))
--check to see if in already!


--also need version where it checks if already in.
exportProp :: Node -> Node -> Tactic
exportProp n n2 = funToRunner (\(md,state,e) ->
  let
    --get the treeIndex tree index corresponding to n
    ti = getTreeIndex n md
    --get the variables, assumptions, and conclusions of the treeIndex
    bContext = getByTreeIndex ti (contextTree md)
    --the vars
    assmsNodes = vars bContext
    --the assms
    varNodes = assms bContext
    --get the formula at n
    f = getProp n md
    --put in the implies (alternatively make this a list!)
    f1 = foldl (\form -> (\m -> (graft "implies" [getProp m md, form]))) f assmsNodes
    --put in the foralls
    f2 = foldl (\form -> (\m -> (graft "forall" [getProp m md, form]))) f1 varNodes
    --shave off the innermost treeIndex
    ti2 = take ((length ti) - 1) ti
    --insert the proposition
  in
    (md -: insertProp n2 f2
	-: insertEdges n2 [n]
	-: setTreeIndex n2 ti2
	-: setHowConclude n2 "exportProp", 
     state -: set "message" "Exported propsition"
		-: set "changed" [n2], e))


--right now this unfolds all
unfoldProp :: Node -> [Node] -> Tactic
unfoldProp n ns = funToRunner (\(md, state, e) ->
  let
    p = props md
    c = contextTree md
    fi = formulaInfo md
    f = getProp n md
    --unfold forall to get variables
    (f1, vars, _) = (Runner.run unfoldAll) f
    (f2, forms, _) = (Runner.run unfoldAllImplies) f1

    varFs = fmap (\x-> Node x []) vars
    
    varNodes = take (length vars) ns
    premNodes= sublist (length vars) (length vars + length forms) ns
    conclNode= ns !! (length vars + length forms)
    allNodes = conclNode:(take (length vars + length forms) ns)
    
    --find the index for n
    ti = getTreeIndex n md
    --create a treeIndex. this gives a new ctree and the tree index
    (ctree, ti2) = addTreeIndex ti varNodes premNodes c
  in 
   if vars==[] && forms==[] 
    then (md, state -: set "message" "Nothing to unfold!", Fail)
    else (md -: setContextTree ctree
	-: insertProps varNodes (reverse varFs)
	-: insertProps premNodes (reverse forms)
	-: insertProp conclNode f2
	-: insertEdges n [conclNode]
	-: setHowConclude n "exportProp"
	-: setTreeIndices allNodes ti2, 
     state -: set "message" "Unfolded proposition"
	-: set "changed" allNodes, e))

--not keeping track of variable dependencies right now.

