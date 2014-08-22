{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XScopedTypeVariables
#-}

module Pattern ( Pattern.apply, unfoldForAll, unfoldAll, unfoldImplies, unfoldAllImplies, tfold, makeSub, findSubs) where
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

import Utilities
import Runner
import Type
import TreeParser

--apply first formula to second
--apply (P, (P->Q,..)) gives (Q, (?1=P, ?2=Q),...).
apply :: Formula -> Pattern Substitution
apply f1 = impliesPattern 
           .> (failIf (\(x,sub) -> (Map.lookup "?1" sub /= Just f1)))
           
--apply a list of formulas
applyList:: [Formula] -> Pattern Substitution
applyList li = joins (fmap Pattern.apply li)
           
--define how to find a substitution
--unfolds one "forall"
unfoldForAll :: Pattern [Symbol]
unfoldForAll = 
  wrapRunner2 (\li -> \sub -> (root (lookup2 "?1" sub):li)) forallPattern
--take the result of forAllPattern and add the variable to the list.

unfoldAll :: Pattern [Symbol]
unfoldAll = many unfoldForAll

--unfold all implies: turns P=>Q=>R into R, [Q,P], etc.
unfoldImplies :: Pattern [Formula]
unfoldImplies = 
  wrapRunner2 (\li -> \sub -> ((lookup2 "?1" sub):li)) impliesPattern
--take the result of forAllPattern and add the variable to the list.

unfoldAllImplies :: Pattern [Formula]
unfoldAllImplies = many unfoldImplies

tfold :: (a -> [b] -> b) -> Tree a -> b
tfold f t = 
  f (root t) ([tfold f c | c <- children t])
  --can implement dispExpr with tfold

makeSub:: Pattern Substitution
makeSub = funToRunner (\(f, subs, e) -> 
  let 
    g = (\rt -> (\clist -> (
      if (rt `Map.member` subs) 
      --if it's to be substituted
        then 
          lookup2 (rt) subs
          --return the substituted
        else
          graft rt clist
          -- then look up in substitution and replace
      )))
  in
    ((tfold:: (Symbol -> [Formula] -> Formula) -> Formula -> Formula) g f, subs,e))

instance Pointed (Substitution, [Symbol]) where
  point = (point, [])

--revMatch f1 on (f2, sub) sees if, f1 is in the form of f2 given the constraint given by the substitutions.
--example: f2=x+0. f1=(y+z)+0. will set x->y+z. If x->w already, and w/=y+z, then fails. 
revMatch:: Formula -> Pattern (Substitution, [Symbol])
revMatch f1 = funToRunner (\(f2, (sub, vars), e) ->
  let
    (_,sub2,e2) = (fun (formulaToPattern2 vars f2) (f1, sub, e))
  in
    if e2/=Fail
      then (f2, (sub2, vars), OK)
      else (f2, (sub, vars), Fail))

findSubs :: [Formula] -> Pattern (Substitution, [Symbol])
findSubs fs = Runner point (\(f,(sub, syms), e) -> 
  let
    unfoldAllWrapper = wrapRunner2 (\(sub1, syms1)-> \syms2-> (sub1, syms2)) unfoldAll
    --check that the LHS is p, and keeps track of substitutions.
    (f1, (sub1, syms1), e1) = (fun unfoldAllWrapper) (f,(sub, syms), e)
    checkPimpliesQ = (\f2 -> 
--discard the output of matchJustSymbol.
      ((matchJustSymbol "implies")::(Pattern (Substitution,[Symbol])))
        .> 
          ((nthChild 0 .> (revMatch f2)) 
            `origJoin` (nthChild 1)))
    --(1) check to see the root is "implies". (2) check to see the left is the non-substitued version of the formula, (3) move to the right child.
    pattern = (joins (fmap checkPimpliesQ fs)) .> (wrapRunner (\(x1,(y1,_)) _ -> (x1,y1,OK)) (\(x1,(y1,z1)) (x2,y2,e2) -> (x2,(y1,z1),e2)) makeSub)
    --load the variables in. 
    --checkAssmsWrapper = wrapRunner (\(tree, (sub, syms)) -> (\defaultSub -> (tree, defaultSub, OK))) (\(tree, (sub, syms)) -> (\(tree2, sub2, e) -> (tree2, (sub2, syms), e))) checkAssms
    (f2, (sub2,_), e2) = (fun pattern) (f1, (sub1, syms1), e1)
  in
    if e1==Fail 
      then (f1, (sub1, syms1), e1) 
      else (f2, (sub2, syms1), e2))

-- pruneLeft:: Tree a -> (Tree a, Tree a)
-- pruneLeft ta = 
--   let
--     r = root ta
--     cs = children ta
--     (t1,t2) = splitAt 1 cs
--   in
--     (t1, graft r t2)
    
--unsafe
--parseToSubs:: String -> Formula -> Substitution
--parseToSubs str f = getCont ((parsePattern str) (formulaToData f))

-- parseToList:: [String] -> String -> Formula -> Maybe [Formula]
-- parseToList vars str f = 
--   let
--     li = lookupList vars (parseToSubs str f) 
--   in
--     if (Nothing `elem` li) then Nothing else Just (fmap removeJust li)

-- unfoldVar :: Pattern [Formula] -> String -> Pattern [Formula]
-- unfoldVar p str = Runner point (\(f,fs,e)
--   let 
--     image = p (f,Map.empty,e)
--     (newF,newS,newE) = image
--   in 
--     case (newE) of 
--       Fail -> image
--       _ -> (newF, newE, (lookup2 str (newS)):fs)

-- unfoldN :: String -> String -> Formula -> Maybe (Formula, [Formula])
-- unfoldN p str f = 
--   let (resF, resE, resFs) = count n (unfoldVar (parsePattern p) str) (f,OK,[])
--   in
--     if (resE == Fail)
--       then Nothing
--       else Just (resF, resFs)

-- typeCheck:: Formula -> Formula -> Substitution -> SymbolLib -> Bool
-- typeCheck f ty sub lib = 
--   let 
--     r = root f
--     rSym = graft r []
--     cs = children f
--     symInfo = lookup r lib 
--     symType = typ symInfo
--   in case r of
--     "lambda" -> --lambda
--       let 
--         [c1,c2,c3] = cs
--         lr = parseToList ["?left,?right"] "->(?left,?right)" ty
--         --newSub = insert (root c1) c2 sub
--       in
--         case lr of 
--           Nothing -> False
--           Just [left, right] -> (c2 == left) && (typeCheck c3 right newSub)
--       --parseToList ["?var","?typ","?fun"] "lambda(?var,?typ,?fun)" 
--     str -> 
--       let 
--         mStrType = 
--           Map.lookup str sub ++ Map.lookup str lib
--       in
--         case mStrType of
--           Nothing -> False --unknown type
--           Just strType -> --strType is the type of the looked-up symbol.
--           --given A->B->C->... we extract the left n
--             let
--               n = length cs 
--               --n=0 means no children, we're done.
--               mLeftList = unfoldN n "->(?1,?2)" "?1" strType
--             in case mLeftList of
--               Nothing -> False --too few arguments
--               Just (remaining, leftList) -> 
--                 (remaining == strType) && and (fmap (\(xTerm,xType)-> typeCheck xTerm xType sub lib) (zip cs leftList))
-- --change to use rewriting rule, etc.
