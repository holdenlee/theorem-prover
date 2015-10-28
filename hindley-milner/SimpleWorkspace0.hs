{-# OPTIONS

  -XExistentialQuantification
  -XRank2Types
#-}

{-# LANGUAGE TemplateHaskell #-}

module SimpleWorkspace where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Control.Lens
import Data.Maybe
import Data.Tree
import Control.Monad.State.Lazy
import Pointed

--simplest workspace for now
data Workspace a = Workspace {_vars :: M.Map String a, _funs :: M.Map String ([a] -> a)}

instance Pointed (Workspace a) where
  point = Workspace M.empty M.empty

makeLenses ''Workspace

instance (Show a) => Show (Workspace a) where
  show w = intercalate "\n" $ map (\(x,y) -> x ++ ": " ++ (show y)) (M.toList $ _vars w)

unionW :: Workspace a -> Workspace a -> Workspace a
unionW w1 w2 = Workspace (M.union (_vars w1) (_vars w2)) (M.union (_funs w1) (_funs w2))

--data LTree l a = Leaf a | Node l [LTree l a]

plus x y = Node "plus" [x,y]
plus' [x,y] = x+y

neg x = Node "neg" [x]
neg' [x] = -x

intfs = M.fromList [("plus", plus'),
                    ("neg", neg')]

--lookup2 k m = fromJust $ M.lookup k m

eval :: Workspace a -> Tree String -> a
eval w t = case t of
            Node x [] -> (M.!) (_vars w) x
            Node l xs -> (M.!) (_funs w) l (map (eval w) xs)

define :: String -> Workspace a -> Tree String -> Workspace a
define name w t = w & vars %~ (M.insert name (eval w t))

define2 :: String -> Workspace a -> Tree String -> (Tree String, Workspace a)
define2 name w t = ((Node name []),  w & vars %~ (M.insert name (eval w t)))

w0 = Workspace (M.singleton "a" 1) intfs
a = Node "a" []
{-
(b, w1) = define2 "b" w0 (plus a a)
(c, w2) = define2 "c" w1 (plus a b)
(d, w3) = define2 "d" w2 (neg (plus (plus a a) b))
-}
--clunky

define' :: String -> Tree String -> State (Workspace a) (Tree String)
define' name t = do
  modify (\w -> w & vars %~ (M.insert name (eval w t)))
  return (Node name [])  

wstate :: State (Workspace a) () -- (Tree String)
wstate = do
  b <- define' "b" (plus a a)
  c <- define' "c" (plus a b)
  e <- define' "e" (neg b)
  d <- define' "d" (neg (plus (plus a b) c))
  return ()

finalState = execState wstate w0

{-
newtype WS a = WS (a, Workspace a)

instance Functor (WS a) where
  fmap f (WS (x, w)) = WS (f x, w)

instance Applicative (WS a) where
  pure x = WS (x, point)
  (WS (f, _)) <*> WS (x, w) = WS (f x, w)

instance Monad (WS a) where
  x >>= f = 
-}
-- (a -> State s b) -> (State s a -> State s b)
-- (a -> (b, Workspace b)) -> (a, Workspace a) -> (b, Workspace b)
