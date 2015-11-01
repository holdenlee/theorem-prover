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
import Control.Monad.Trans.RWS.Lazy --or Strict?
import Control.Monad.Trans.Maybe
import Data.Monoid
import Control.Lens
import Control.Applicative

(.&) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
(.&) = (>=>)

--how to write point-free? (or template haskell...)
c2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
c2 f g x y = f $ g x y

--(Monoid w) => Int -> (Tactic r w s a) -> (Tactic r w s a)
repeatT :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
repeatT = foldl1 (.&) `c2` replicate
