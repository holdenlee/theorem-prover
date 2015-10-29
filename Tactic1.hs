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

(.&) :: (w -> Tactic c x) -> (x -> Tactic c y) -> (w -> Tactic c y)
(.&) = (>=>)

(.|) :: (w -> Tactic c x) -> (w -> Tactic c x) -> (w -> Tactic c x)
(.|) f g x = ListT $ (++) <$> (runListT $ f x) <*> (runListT $ g x)

try :: (w -> Tactic c w) -> (w -> Tactic c w)
try f = f .| return
