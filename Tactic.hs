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

module Tactic where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Data.Monoid

--Reader is commutative
--context, log, and workspace
type Tactic c l w = WriterT l (ListT (Reader c)) w

instance (Monad m) => Monoid (ListT m a) where
    mempty = ListT $ return []
    mappend t1 t2 = ListT $ (++) <$> (runListT t1) <*> (runListT t2)

{-
instance (Monoid l) => Monoid (Tactic c l w) where
  mempty = runWriterT-}

instance (Monad m, Monoid l, Monoid (m (w, l))) => Monoid (WriterT l m w) where
  mempty = WriterT $ mempty
  mappend t1 t2 = WriterT $ (runWriterT t1) <> (runWriterT t2)

(.&) :: (Monoid l) => (w -> Tactic c l x) -> (x -> Tactic c l y) -> (w -> Tactic c l y)
(.&) = (>=>)

(.|) :: (Monoid l) => (w -> Tactic c l x) -> (w -> Tactic c l x) -> (w -> Tactic c l x)
(.|) f g x = (f x) <> (g x)

try :: (Monoid l) => (w -> Tactic c l w) -> (w -> Tactic c l w)
try f = f .| return
