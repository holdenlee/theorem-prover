{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XFlexibleContexts
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XUndecidableInstances
#-}

module NTactic where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Lens

import Utilities

--Reader is commutative
--context, log, and workspace
type NProofState c l w = WriterT l (ListT (Reader c)) w

type Tactic' c l w x = (w -> NProofState c l x)

type Tactic c l w = Tactic' c l w w

instance (Monad m) => Monoid (ListT m a) where
    mempty = ListT $ return []
    mappend t1 t2 = ListT $ (++) <$> (runListT t1) <*> (runListT t2)

instance (Monad m, Monoid l, Monoid (m (w, l))) => Monoid (WriterT l m w) where
  mempty = WriterT $ mempty
  mappend t1 t2 = WriterT $ (runWriterT t1) <> (runWriterT t2)

{-
(.&) :: (Monoid l) => (w -> NProofState c l x) -> (x -> NProofState c l y) -> (w -> NProofState c l y)
(.&) = (>=>)-}

(.|) :: (Monoid l) => (w -> NProofState c l x) -> (w -> NProofState c l x) -> (w -> NProofState c l x)
(.|) f g x = (f x) <> (g x)

try :: (Monoid l) => (w -> NProofState c l w) -> (w -> NProofState c l w)
try f = f .| return

nProofState :: (Monoid l) => (c -> [(w, l)]) -> NProofState c l w
nProofState f = do
  c <- ask --ask for the context
  WriterT $ ListT $ return (f c)

runNProofState :: (NProofState c l w) -> (c -> [(w, l)])
runNProofState = runReader . runListT . runWriterT

evalNProofState :: NProofState c l w -> c -> (w, l)
evalNProofState = (!!0) `c2` runNProofState 

class HasStatus a where
  succeeded :: a -> Bool

--assume techniques always check for doneness.
  {-
repeatUntilDone' :: (HasStatus w) => (w -> [(w,l)]) -> (w -> [(w,l)])
repeatUntilDone' f w =
  case f w of
   [] -> []
   (hw,hl):rest -> if succeeded hw then (hw,hl):(repeatUntilDone' (f w)
-}
