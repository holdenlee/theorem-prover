{-# LANGUAGE LambdaCase, TemplateHaskell #-}

{-# OPTIONS
    -XTemplateHaskell
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XTypeSynonymInstances
    -XFlexibleInstances
#-}

module Tableau where

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Control.Lens hiding (Context, (|>), contexts)
import Control.Monad
import Control.Monad.Free

import CmdLineSetup
import PMatchT
import Prop
import IOUtils
import Utilities

data Proof' a = Proof' String [Prop] [a]

instance Functor Proof' where
  fmap f (Proof' str li li2) = Proof' str li (fmap f li2)

type Proof = Free Proof' Int

{-| A context is a list of assumptions (and things shown) and a conclusion to try to prove.-}
data Context = Context {_contextAssms :: S.Set Int, _contextConcl :: S.Set Int}

{-| A workspace is a list of statements with partial proofs, contexts, the current context (which has focus), the unproven contexts, and a supply of fresh numbers. -}
data Workspace = Workspace {_statements :: M.Map Int Prop, _proofs :: M.Map Int Proof, _contexts :: [Context], _cur :: Int, _unproven :: S.Set Int, _num :: Int}

makeFields ''Context
makeLenses ''Workspace

mcompose :: (Ord a, Monad m) => M.Map a (m a) -> M.Map a (m a) -> M.Map a (m a)
mcompose m1 m2 =
  let
    --g :: (a -> Maybe (m a)) -> m a -> m a
    g f x = do
      x' <- x
      case f x' of
       Nothing -> x
       Just y -> y
  in
   (M.map (g (flip M.lookup m1)) m2) `M.union` m1

addProof :: M.Map Int Proof -> Workspace -> Workspace
addProof m w = w & proofs %~ (`mcompose` m)

addDummyProof :: Int -> Workspace -> Workspace
addDummyProof n = addProof (M.singleton n (Pure n))

switchContext :: Int -> Workspace -> Workspace
switchContext n w = if (n>=0 && n < length (_contexts w))
                    then w & cur .~ n
                    else w

findStmt :: Prop -> Int -> Workspace -> Maybe Int
findStmt p i w =
  S.map (appendFun ((w ^. statements) M.!)) (w ^. contexts . ix i . assms) |>
  S.toList |>
  filter ((==p) . snd) |>
  fmap fst |>
  listToMaybe

findCurStmt p i w = findStmt p (w ^. cur) w

findCurGoal p i w = findGoal p (w ^. cur) w

findGoal :: Prop -> Int -> Workspace -> Maybe Int
findGoal p i w =
  S.map (appendFun ((w ^. statements) M.!)) (w ^. contexts . ix i . concl) |>
  S.toList |>
  filter ((==p) . snd) |>
  fmap fst |>
  listToMaybe

addStmt :: Prop -> Int -> Workspace -> Workspace
addStmt p i w =
  let n = w ^. num
  in w & statements . ix n .~ p
       & contexts . ix i . assms %~ S.insert n
       & incrementNum
--       & proofs . at num .~ pf
       --assms should be called knowns

addStmtAtCur p w = addStmt p (w ^. cur) w

addConcl :: Prop -> Int -> Workspace -> Workspace
addConcl p i w =
  let n = w ^. num
  in w & statements . ix n .~ p
       & contexts . ix i . concl %~ S.insert n
       & incrementNum
--       & proofs . at num .~ Pure num
       --assms should be called knowns

addConclAtCur p w = addConcl p (w ^. cur) w

removeConcl :: Int -> Int -> Workspace -> Workspace
removeConcl j i w =
  let n = w ^. num
  in w & contexts . ix i . concl %~ S.delete j
--       & proofs . at num .~ Pure num
       --assms should be called knowns

removeConcl j i w = addConcl j (w ^. cur) w


--should make this a lens
curContext :: Workspace -> Context
curContext w = fromJust (w ^? contexts . ix (w ^. cur))

curAssms :: Workspace -> [(Int, Prop)]
curAssms w = map (appendFun $ ((w ^. statements) M.!)) (S.toList ((curContext w) ^. assms))

curConcl :: Workspace -> [(Int, Prop)]
curConcl w = map (appendFun $ ((w ^. statements) M.!)) (S.toList ((curContext w) ^. concl))

moveConclToKnown :: Int -> Workspace -> Workspace
moveConclToKnown n = w & contexts . ix (w ^. cur) . concl %~ (S.delete n)
                       & contexts . ix (w ^. cur) . assms %~ (S.insert n)

changeContextIfDone :: Workspace -> Workspace
changeContextIfDone w = if null (curConcl w)
                        then
                          let 
                            unp = w ^. unproven
                            unp' = S.delete (w ^. cur) unp'
                            m = if S.empty unp' then -1 else S.findMin unp'
                          in
                            w & unproven .~ unp' 
                              & cur .~ m
                        else w

incrementNum :: Workspace -> Workspace
incrementNum = num %~ (+1)

copyContextReplacingConcl :: Int -> [Prop] -> Workspace -> Workspace
copyContextReplacingConcl i prs w =
  let
    n = w ^. num
    news = [n..(n + (length prs) - 1)]
  in
   w & contexts %~ (++[(w ^. contexts . ix i) & concl .~ (S.fromList news)])
     & statements %~ (foldIterate M.insert (zipWith news prs))
     & foldIterate addDummyProof [n..(n + (length prs) - 1)]
     & unproven %~ (S.insert (length (w ^. contexts)))
     & incrementNum

copyCurContextReplacingConcl pr w = copyContextReplacingConcl (w ^. cur) pr w

forwardReason dr s li = tryDo (forwardReason' dr s li)

forwardReason' :: DeductionRule -> [(Int,Prop)] -> [Int] -> Workspace -> Maybe Workspace
forwardReason' dr s li w = do
  let n = w ^. num
  let stmtsInContext = (S.fromList li) `S.isSubsetOf` (S.fromList $ map fst $ curAssms w)
  let stmts = (map ((w ^. statements) M.!) li)
  let drAssms = dr ^. assms
  let drConcl = dr ^. concl
  let liLen = length li
  (s2, con) <- forward s drAssms stmts drConcl
  guard stmtsInContext
  let addlAssms = (map (sub s2) (drop liLen drAssms))
          -- assumptions not included
  let allVarsInst = S.fromList [1..(dr ^. args)] `S.isSubsetOf` (S.fromList $ M.keys s2)
      --have we instantiated all the variables?
  guard allVarsInst
  guard (null addlAssms) -- this makes things much simpler.
  let arguments1 = map (Prop' . PVar) (M.keys s2)
  let arguments2 = map Pure li
  return (w -- - |> copyCurContextReplacingConcl addlAssms
            |> addStmtAtCur conw
            |> (addProof $ M.singleton (n+(length addlAssms)+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
            |> changeContextIfDone)
      {-
  let doToW = case (findGoal con w) of
                   Nothing -> addStmtAtCur conw
                    >> incrementNum
                    >> (addProof $ M.singleton (n+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
                   Just i -> (addProof $ M.singleton i (Free (Proof' (dr ^. name) arguments1 arguments2)))
                    >> moveConclToKnown i
  return (w |> doToW)-}
{-
let arguments1 = map (Prop' . PVar) (M.keys s2)
  let arguments2 = map Pure (li++[w ^. num, (w ^. num) + (length addlAssms) - 1])
  let doToW = case (findGoal con w) of
                   Nothing -> addStmtAtCur con
                    >> incrementNum
                    >> (addProof $ M.singleton (n+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
                    >>foldIterate copyCurContextReplacingConcl addlAssms
                   Just i -> (addProof $ M.singleton (n+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
                    >> moveConclToKnown i
                    >> foldIterate copyCurContextReplacingConcl addlAssms
  return (w |> doToW)-}

backwardReason dr s cnum = tryDo (backwardReason' dr s cnum)

--[(Int, Prop)] -> [Prop] -> Prop -> Prop -> Maybe (M.Map Int Prop, [Prop])
backwardReason' :: DeductionRule -> [(Int,Prop)] -> Int -> Workspace -> Maybe Workspace
backwardReason' dr s cnum w = do
  let n = w ^. num
  let goalInContext = cnum `elem` (map fst $ curConcl w)
  let conc = (w ^. statements) M.! cnum
  let drAssms = dr ^. assms
  let drConcl = dr ^. concl
  let liLen = length li
  (s2, subbedKnown) <- backward s drAssms drConcl conc
  guard goalInContext
          -- assumptions not included
  let allVarsInst = S.fromList [1..(dr ^. args)] `S.isSubsetOf` (S.fromList $ M.keys s2)
      --have we instantiated all the variables?
  let withMaybeIndices = map (appendFun $ findCurStmt) subbedKnown --need to contain indices!!!
  let (ct, newList, toAdd) = 
                for withMaybeIndices (1, [], []) (\(p, maybeIndex) (i, li', li2) -> case maybeIndex of
                                                                            Nothing -> (i+1, li'++[(p, n+i)], li2++[p])
                                                                            Just y -> (i, li'++[(p, y)], li2)
--  let (found, notFound) = partition (isJust . snd) withMaybeIndices
  guard allVarsInst
  let arguments1 = map (Prop' . PVar) (M.keys s2)
  let arguments2 = map (Pure . snd) newList
  if length (curConcl w) == 1
     then w & addProof (M.singleton (n+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
            & removeConclAtCur cnum
            & foldIterate addConclAtCur toAdd
     else w & addProof (M.singleton (n+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
            & copyCurContextReplacingConcl toAdd
            & removeConclAtCur cnum
