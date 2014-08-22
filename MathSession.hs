{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

{-mathDAG :: MathDAG
                 , allKnown :: [Node]
                 , currentKnown :: [Node]
                 , currentGoals :: [Node]
                 , library :: Map String Formula
		 , symbolLib -}
module MathSession ( MathSession (library, currentGoals), setMathDAG, dest, dests, destG, destGs, insertKnown, insertGoal, insertKnowns, insertGoals, intro, loadLibrary, constructLibrary, initSession, Tactical, forwardReason2, unfoldProp2, exportProp2, showMS, showLibrary) where
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
import TreeParser
import MathDAG
import Pattern
import State
import Tactics

--MathSession
data MathSession = MathSession { mathDAG :: MathDAG
                 , allKnown :: [Node]
                 , currentKnown :: [Node]
                 , currentGoals :: [Node]
                 , library :: Map.Map String Formula
		 , symbolLib :: SymbolLib}

type Tactical = Runner MathSession MathSession State

setMathDAG:: MathDAG -> MathSession -> MathSession
setMathDAG md ms = ms{mathDAG=md}

setSymbolLib:: SymbolLib -> MathSession -> MathSession
setSymbolLib slib ms = ms{symbolLib=slib}

dest :: Node -> MathSession -> MathSession
dest n ms = ms{currentKnown = Data.List.delete n (currentKnown ms)}

dests :: [Node] -> MathSession -> MathSession
dests ns ms = ms{currentKnown = foldl (\ck -> (\n -> Data.List.delete n ck)) (currentKnown ms) ns}

destG :: Node -> MathSession -> MathSession
destG n ms = ms{currentGoals = Data.List.delete n (currentGoals ms)}

destGs :: [Node] -> MathSession -> MathSession
destGs ns ms = ms{currentGoals = foldl (\ck -> (\n -> Data.List.delete n ck)) (currentGoals ms) ns}

insertKnown :: Node -> MathSession -> MathSession
insertKnown n ms = ms{currentKnown = (n:currentKnown ms)}

insertGoal :: Node -> MathSession -> MathSession
insertGoal n ms = ms{currentGoals = (n:currentGoals ms)}

insertKnowns :: [Node] -> MathSession -> MathSession
insertKnowns ns ms = ms{currentKnown = (ns ++ currentKnown ms)}

insertGoals :: [Node] -> MathSession -> MathSession
insertGoals ns ms = ms{currentGoals = (ns ++ currentGoals ms)}

intro :: Node -> MathSession -> MathSession
intro n ms = 
  let
    pres = pre (props (mathDAG ms)) n
    ak = allKnown ms
    cg = currentGoals ms
    ms2= destG n ms
  in -- if n is already known, then don't add it to current goals
    foldl (\m -> (\n -> if (elem n ak) then m else insertGoal n m)) ms2 pres

instance Pointed MathSession where
  point = MathSession point [] [] [] Map.empty Map.empty

loadLibrary :: String -> [(String, Formula)]
loadLibrary str = 
  let 
    fs = lines str
    g = (\f ->
      let 
        ftree = (parseFormula f)
        (varNode:form:_) = children ftree
      in 
        ((root varNode), form)
      )
  in 
    fmap g fs    
  --in form prop(propName,implies(or(P,Q),P)), for instance
  --parse them as formula trees using funParse. 

constructLibrary :: String -> MathSession -> MathSession
constructLibrary str ms = ms{library = insertMultiple (loadLibrary str) (library ms)}

initSession :: String -> SymbolLib -> String -> MathSession 
initSession lib slib ftext = 
	point -: constructLibrary lib 
		-: setMathDAG (initMathDAG (parseFormula ftext))
		-: setSymbolLib slib
		-: insertGoal 1

--start tactics
forwardReason2 :: [Node] -> Node -> String -> Bool -> Tactical
forwardReason2 hyps goal str des = funToRunner (\(ms, state, e) ->
  let 
    f = lookup2 str (library ms)  --dangerous, replace this.
    md = mathDAG ms
    (md2, ps, e) = (Runner.run (forwardReason f hyps goal)) md
    --g = (get "changed" ps) !! 0
  in
    if (e /= Fail) 
      then
	if (get "message" ps =="Proved proposition")
	  then (ms{mathDAG = md2} -: destG goal,state -: set "message" ((get "message" ps)::String) , OK)
          else (ms{mathDAG = md2} 
               -: doIf des (dests hyps) 
               -: insertKnown goal, 
               state -: set "message" ((get "message" ps)::String) , OK) --g
      else
        (ms,state -: set "message" ((get "message" ps)::String) ,Fail))


unfoldProp2 :: Node -> [Node] -> Tactical
unfoldProp2 n ns = funToRunner (\(ms, state, _) ->
  let 
    md = mathDAG ms
    (md2, ps, e) = (Runner.run (unfoldProp n ns)) md
    c= get "changed" ps
    cnode = c !! 0
    pnodes = drop 1 c
  in
    if (e/=Fail)
      then (ms{mathDAG = md2} -: insertKnowns pnodes -: insertGoal cnode -: destG n, state -: set "message" ((get "message" ps)::String) ,OK)
      else (ms,state -: set "message" ((get "message" ps)::String) ,Fail))

--todo: if bool is true, then destroy context.

exportProp2 :: Node -> Node -> Bool -> Tactical
exportProp2 n n2 des  = funToRunner (\(ms,state,_) ->
  let 
    md = mathDAG ms
    (md2, ps, e) = (Runner.run (exportProp n n2)) md
    cnode = (get "changed" ps) !! 0
  in
    if (e/=Fail)
      then (ms{mathDAG = md2} -: insertKnown cnode -: doIf (des) (dest n), state -: set "message" ((get "message" ps)::String) ,OK)
      else (ms,state -: set "message" ((get "message" ps)::String) ,Fail))

showMS :: MathSession -> String
showMS ms = 
    let 
      md = mathDAG ms
      showKnown = unlines [showProp (symbolLib ms) n md | n <- currentKnown ms]    
      line = "-----------------------------\n"
      showGoals = unlines [showProp (symbolLib ms) n md | n <- currentGoals ms]
    in (showKnown ++ line ++ showGoals)

showLibrary :: MathSession -> String
showLibrary ms = 
  let 
    lib = library ms
    slib = symbolLib ms
  in
    unlines (fmap (\k -> (k ++ ": " ++ (show2 slib (lookup2 k lib)))) (Data.List.sort (Map.keys (lib))))

