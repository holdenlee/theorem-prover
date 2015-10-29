{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Tactic1 where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.List
import Data.Monoid

--Reader is commutative
type Tactic c w = ListT (Reader c) w

instance (Monad m) => Monoid (ListT m a) where
    mempty = ListT $ return []
    mappend t1 t2 = ListT $ (++) <$> (runListT t1) <*> (runListT t2)

(.&) :: (w -> Tactic c x) -> (x -> Tactic c y) -> (w -> Tactic c y)
(.&) = (>=>)

(.|) :: (w -> Tactic c x) -> (w -> Tactic c x) -> (w -> Tactic c x)
(.|) f g x = (f x) <> (g x)

try :: (w -> Tactic c w) -> (w -> Tactic c w)
try f = f .| return
