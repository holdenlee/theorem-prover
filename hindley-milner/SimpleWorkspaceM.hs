{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module SimpleWorkspaceM where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Control.Lens
import Data.Maybe
import Data.Tree
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Pointed

--simplest workspace for now
data Workspace a = Workspace {_vars :: M.Map String a, _funs :: M.Map String ([a] -> Maybe a)}

instance Pointed (Workspace a) where
  point = Workspace M.empty M.empty

makeLenses ''Workspace

instance (Show a) => Show (Workspace a) where
  show w = intercalate "\n" $ map (\(x,y) -> x ++ ": " ++ (show y)) (M.toList $ _vars w)

unionW :: Workspace a -> Workspace a -> Workspace a
unionW w1 w2 = Workspace (M.union (_vars w1) (_vars w2)) (M.union (_funs w1) (_funs w2))

plus x y = Node "plus" [x,y]
plus' [x,y] = Just (x+y)

neg x = Node "neg" [x]
neg' [x] = Just (-x)

divide x y = Node "divide" [x,y]
divide' [x,y] = if y==0 then Nothing else Just (x `div` y)

intfs = M.fromList [("plus", plus'),
                    ("neg", neg'),
                    ("divide",divide')]

eval :: Workspace a -> Tree String -> Maybe a
eval w t = case t of
            Node x [] -> M.lookup x (_vars w)
            Node l xs -> do
              args <- mapM (eval w) xs
              --sequence $ map (eval w) xs -- :: [Maybe a]
              f <- M.lookup l (_funs w)
              f args

w0 = Workspace (M.singleton "a" 1) intfs
a = Node "a" []

define :: String -> Tree String -> MaybeT (State (Workspace a)) (Tree String)
define name t = MaybeT $ do
  w <- get
  let y = eval w t
  case y of
   Nothing -> return Nothing
   Just y' -> do
     put (w & vars %~ (M.insert name y'))
     return (Just (Node name []))
  {-do
  w <- get
  y <- return $ eval w t
  put (w & vars %~ (M.insert name (eval w t)))
  return (Node name [])-}

{-
define :: String -> Tree String -> StateT (Workspace a) Maybe (Tree String)
define name t = StateT (\w -> do
                           y <- eval w t
                           return (Node name []-}
{-
  eval w t
  modify (\w -> w & vars %~ (M.insert name (eval w t)))
  return (Node name [])  -}

type MSWorkspace a = MaybeT (State (Workspace a))

wstate :: MSWorkspace a () -- (Tree String)
wstate = do
  b <- define "b" (neg a)
  c <- define "c" (plus a b)
  d <- define "d" (divide a c)
  e <- define "e" (plus c c)
  return ()

finalState = execState (runMaybeT wstate) w0

execProof proof w0 = execState (runMaybeT proof) w0
