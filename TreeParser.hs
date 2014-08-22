{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module TreeParser ( joinWith, Pattern, matchJustSymbol, matchVar, graftPattern, formulaToPattern, formulaToPattern2, parsePattern, impliesPattern, forallPattern, nthChild, extractVars, clearQs, parseFormula) where
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

import Runner
import Type
import Utilities

instance Pointed [a] where 
  point = []
  
instance Pointed (Map.Map k v) where
  point = Map.empty

type Pattern c = Runner Formula Formula c

--Pattern Substitution is good enough.
--a=Tree b
matchJustSymbol :: (Pointed c) => Symbol -> Pattern c
matchJustSymbol sym = check (\x -> root x == sym)

matchVar :: Symbol -> Pattern Substitution
matchVar x = Runner Map.empty (\(f,s,e)->
  if e==Fail then (f,s,e)
    else (case Map.lookup x s of
      Nothing -> (f, Map.insert x f s, e) --insert the "x=f" in the substitution
      Just f2 ->
        if f2 == f
          then (f,s,e) --"x=f" is already in the substitution
          else (f,s,Fail) --"x=g, g/=f" is in the substitution, so fail.
      )
    )
  
graftPattern:: Pattern c -> [Pattern c] -> Pattern c
graftPattern rootPattern patts = joinWith (\input -> input:(children input)) (rootPattern:patts)

formulaToPattern :: Tree Symbol -> Pattern Substitution
formulaToPattern tr = 
  let 
    symb = root tr
    rootPattern = (case (root tr) of
      '?':_ -> matchVar (symb) --vs. JustVar
      _     -> matchJustSymbol (symb))
    cs = children tr
    patts = fmap formulaToPattern cs
  in
    graftPattern rootPattern patts    

formulaToPattern2 :: [Symbol] -> Tree Symbol -> Pattern Substitution
formulaToPattern2 syms tr =
  let 
    symb = root tr
    rootPattern = 
      if symb `elem` syms
        then matchVar symb
        else matchJustSymbol symb
    cs = children tr
    patts = fmap (formulaToPattern2 syms) cs
  in
    graftPattern rootPattern patts    

parsePattern :: String -> Pattern Substitution
parsePattern s = 
  (formulaToPattern (justRight (parseFun s)))
   --turn into tree
   --forget about error handling
--turn tree of strings into tree of patterns (may not be efficient, but will work)

impliesPattern :: Pattern Substitution   
impliesPattern = parsePattern "implies(?1,?2)" 

forallPattern :: Pattern Substitution 
forallPattern = parsePattern "forall(?1,?2)"

--tree traversal
nthChild:: (Pointed c) => Int -> Pattern c
nthChild n = rmapm (\tree -> mlookup n (children tree))

clearQs :: Pattern Substitution
clearQs = rmap2 ( 
  Map.mapKeys (\str -> case str of {'?':_ -> ""; x -> x})
  *> Map.delete "")

--no error check
--ex. if list is [x], ?1->x ?2->y, gives x->x
extractVars :: [String] -> Pattern Substitution
extractVars l = rmap2 (\sub -> 
  let 
    lookups = fmap (\x -> Map.lookup x sub) l
    vars = fmap (root.removeJust) (filter (not.isNothing) lookups)
    newSub = insertMultiple (zip vars (repeat (Node "" []))) sub
  in
    newSub)
    
parseFormula :: String -> Formula
parseFormula str = justRight (parseFun str)
