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

module Comb where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.RWS.Lazy --or Strict?
import Control.Monad.Trans.Maybe
import Data.Monoid
import Control.Lens
import Control.Applicative

import Utilities

(.&) :: (Monad m) => (m b) -> (m c) -> (m c)
(.&) = (>>)

(.&.) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
(.&.) = (>=>)

--(Monoid w) => Int -> (Tactic r w s a) -> (Tactic r w s a)
repeatT :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
repeatT = foldl1 (.&) `c2` replicate

(.|) :: (Monad m, Alternative m) => m b -> m b -> m b
(.|) = (<|>)

(.|.) :: (Monad m, Alternative m) => (a -> m b) -> (a -> m b) -> (a -> m b)
(.|.) f g x = (f x) <|> (g x)

try :: (Monad m, Alternative m) => (a -> m a) -> (a -> m a)
try f = f .|. return
  
