{-# LANGUAGE LambdaCase, TemplateHaskell #-}

{-# OPTIONS
    -XTemplateHaskell
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XTypeSynonymInstances
    -XFlexibleInstances
    -XQuasiQuotes
#-}

module Tableau where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Control.Lens hiding (Context, (|>), contexts)
import Control.Monad
import Control.Monad.Free
import Text.Printf
import Prelude -- necessary for Hint.

import PMatchT
import Prop
import IOUtils
import Utilities
import Pointed
import QQ

data Proof' b a = Proof' String [Prop] [a] | FunP b a

instance Functor (Proof' b) where
  fmap f = \case
    (Proof' str li li2) -> Proof' str li (fmap f li2)
    FunP v x -> FunP v (f x)

type Proof = Free (Proof' Int) Int

showProof :: Proof -> String
showProof = \case
  Pure i -> "?"++(show i)
  Free (Proof' str ps pfs) ->
    let
      plist = intercalate " " $ map showProp ps
      pflist = intercalate " " $ map showProof pfs
    in
      printf "(%s %s %s)" str plist pflist
  Free (FunP v x) ->
    printf "(\\%d -> %s)" v (showProof x)

{-| A context is a list of assumptions (and things shown) and a conclusion to try to prove.-}
data Context = Context {_contextAssms :: S.Set Int, _contextConcl :: S.Set Int} deriving (Show, Eq, Ord)

instance Pointed Context where
  point = Context point point

{-| A workspace is a list of statements with partial proofs, contexts, the current context (which has focus), the unproven contexts, and a supply of fresh numbers. -}
data Workspace = Workspace {_statements :: M.Map Int Prop, _proofs :: M.Map Int Proof, _contexts :: [Context], _cur :: Int, _unproven :: S.Set Int, _num :: Int}

instance Pointed Workspace where
  point = Workspace point point point point point point

makeFields ''Context
makeLenses ''Workspace

instance Show Workspace where
  show w =
    if S.null (w ^. unproven)
    then "Proved: "++(showProp $ (w ^. statements) M.! 0)
    else
      let
        printStmts = intercalate "\n" .
                     map (\(n, p) -> printf "%d: %s" n (showProp p))
        knownString = printStmts $ curAssms w
        goalString = printStmts $ curConcl w
        curString = "Current context: " ++ (show (w ^. cur))
        unprovenString = "Active contexts: "++ (show (w ^. unproven))
      in
       printf "%s\n==========\n%s\n%s\n%s"
              knownString goalString curString unprovenString

initWorkspace :: [Prop] -> Prop -> Workspace
initWorkspace hyps conc =
  point & num .~ -1
        & contexts .~ [point]
        & unproven .~ S.singleton 0
        & cur .~ 0
        & foldIterate addStmtAtCur hyps
        & addConclAtCur conc
        & foldIterate addDummyProof [0..(length hyps)]

setWorkspace :: [Prop] -> Prop -> Workspace -> Workspace
setWorkspace hyps conc = const $ initWorkspace hyps conc

mcompose :: (Ord a, Monad m) => M.Map a (m a) -> M.Map a (m a) -> M.Map a (m a)
mcompose m1 m2 =
  let
    --g :: (a -> Maybe (m a)) -> m a -> m a
    g f x = do
      x' <- x
      case f x' of
       Nothing -> return x'
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

findCurStmt p w = findStmt p (w ^. cur) w

findCurGoal p w = findGoal p (w ^. cur) w

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
  in w & statements . at (n+1) .~ Just p
       & contexts . ix i . assms %~ S.insert (n+1)
       & incrementNum
--       & proofs . at num .~ pf
       --assms should be called knowns

addStmtAtCur p w = addStmt p (w ^. cur) w

addConcl :: Prop -> Int -> Workspace -> Workspace
addConcl p i w =
  let n = w ^. num
  in w & statements . at (n+1) .~ Just p
       & contexts . ix i . concl %~ S.insert (n+1)
       & incrementNum
--       & proofs . at num .~ Pure num
       --assms should be called knowns

addConclAtCur p w = addConcl p (w ^. cur) w

removeConcl :: Int -> Int -> Workspace -> Workspace
removeConcl j i w =
  let n = w ^. num
  in w & contexts . ix i . concl %~ S.delete j

removeConclAtCur j w = removeConcl j (w ^. cur) w


--should make this a lens
curContext :: Workspace -> Context
curContext w = fromJust (w ^? contexts . ix (w ^. cur))

curAssms :: Workspace -> [(Int, Prop)]
curAssms w = map (appendFun $ ((w ^. statements) M.!)) (S.toList ((curContext w) ^. assms))

curConcl :: Workspace -> [(Int, Prop)]
curConcl w = map (appendFun $ ((w ^. statements) M.!)) (S.toList ((curContext w) ^. concl))

moveConclToKnown :: Int -> Workspace -> Workspace
moveConclToKnown n w = w & contexts . ix (w ^. cur) . concl %~ (S.delete n)
                         & contexts . ix (w ^. cur) . assms %~ (S.insert n)

changeContextIfDone :: Workspace -> Workspace
changeContextIfDone w = if null (curConcl w)
                        then
                          let 
                            unp = w ^. unproven
                            unp' = S.delete (w ^. cur) unp
                            m = if S.null unp' then -1 else S.findMin unp'
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
    news = [(n+1)..(n + (length prs))]
  in
   w & contexts %~ (++[(fromJust (w ^? contexts . ix i)) & concl .~ (S.fromList news)])
     & statements %~ insertMultiple (zipWith (,) news prs)
     & foldIterate addDummyProof [(n+1)..(n + (length prs))]
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
            |> addStmtAtCur con
            |> (addProof $ M.singleton (n+(length addlAssms)+1) (Free (Proof' (dr ^. name) arguments1 arguments2)))
            |> changeContextIfDone)

backwardReason dr s cnum = tryDo (backwardReason' dr s cnum)

backwardReason' :: DeductionRule -> [(Int,Prop)] -> Int -> Workspace -> Maybe Workspace
backwardReason' dr s cnum w = do
  let n = w ^. num
  let goalInContext = cnum `elem` (map fst $ curConcl w)
  let conc = (w ^. statements) M.! cnum
  let drAssms = dr ^. assms
  let drConcl = dr ^. concl
  (s2, subbedKnown) <- backward s drAssms drConcl conc
  guard goalInContext
          -- assumptions not included
  let allVarsInst = S.fromList [1..(dr ^. args)] `S.isSubsetOf` (S.fromList $ M.keys s2)
      --have we instantiated all the variables?
  let withMaybeIndices = map (appendFun (flip findCurStmt w)) subbedKnown --need to contain indices!!!
  let (ct, newList, toAdd) = for withMaybeIndices (1, [], []) (\(p, maybeIndex) (i, li', li2) -> case maybeIndex of
                                                                                                  Nothing -> (i+1, li'++[(p, n+i)], li2++[p])
                                                                                                  Just y -> (i, li'++[(p, y)], li2))
--  let (found, notFound) = partition (isJust . snd) withMaybeIndices
  guard allVarsInst
  let arguments1 = map (Prop' . PVar) (M.keys s2)
  let arguments2 = map (Pure . snd) newList
  if length (curConcl w) == 1
     then Just $ w & addProof (M.singleton cnum (Free (Proof' (dr ^. name) arguments1 arguments2)))
            & removeConclAtCur cnum
            & foldIterate addConclAtCur toAdd
            & changeContextIfDone
     else Just $ w & addProof (M.singleton cnum (Free (Proof' (dr ^. name) arguments1 arguments2)))
            & copyCurContextReplacingConcl toAdd
            & moveConclToKnown cnum

unfoldReason cnum = tryDo (unfoldReason' cnum)

unfoldReason' :: Int -> Workspace -> Maybe Workspace
unfoldReason' cnum w = do
  let n = w ^. num
  let goalInContext = cnum `elem` (map fst $ curConcl w)
  guard goalInContext
  conc <- M.lookup cnum (w ^. statements)
  subList <- pmatch' [prop|(?1 -> ?2)|] conc
  let [(1,p1), (2,p2)] = subList
  -- (Int, Prop)
  if length (curConcl w) == 1
     then Just $ w & addProof (M.singleton cnum (Free (FunP (n+1) (Pure (n+2)))))
            & addStmtAtCur p1
            & removeConclAtCur cnum
            & addConclAtCur p2
            & addDummyProof (n+1)
            & addDummyProof (n+2)
     else Just $ w & addProof (M.singleton cnum (Free (FunP (n+2) (Pure (n+1)))))
            & copyCurContextReplacingConcl [p2]
            & addStmt p1 (length (w ^. contexts))
            & moveConclToKnown cnum
            & addDummyProof (n+1)
            & addDummyProof (n+2)

ex = [prop|(P -> ((P -> Q) -> ((P -> (Q -> R)) -> R)))|]

exWorkspace = setWorkspace [] ex

{-
exWorkspace undefined
let x = unfoldReason 0 it
showProof ((it ^. proofs) M.! 0)
x
let x = unfoldReason 2 it
showProof ((it ^. proofs) M.! 0)
x
let x = unfoldReason 4 it
showProof ((it ^. proofs) M.! 0)
x
let x = forwardReason mp [] [1,3] it
showProof ((it ^. proofs) M.! 0)
x
let x = forwardReason mp [] [1,5] it
showProof ((it ^. proofs) M.! 0)
x
let x = backwardReason mp [(1,[prop|(Q)|])] 6 it
showProof ((it ^. proofs) M.! 0)

printList $ M.toList $ M.map showProof (x ^. proofs)

let m = mcompose (M.fromList [(0,Pure 0)]) (M.fromList [(0, Free $ FunP 1 (Pure 2))])
showProof (m M.! 0)
-}
