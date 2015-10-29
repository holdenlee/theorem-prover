{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Tactic where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable

newtype Tactic c w = Tactic (Reader c [w])
--maybe I can do this with ListT? But ListT is funky.

_tactic :: Tactic c w -> Reader c [w]
_tactic (Tactic t) = t

instance Functor (Tactic c) where
    fmap f (Tactic r) = Tactic $ fmap (map f) r

instance Applicative (Tactic c) where
    pure x = Tactic $ return [x]
    (Tactic f) <*> (Tactic x) = Tactic $ do
                                  fs <- f
                                  li <- x
                                  return (fs <*> li)

instance Monad (Tactic c) where
    (Tactic x) >>= f = Tactic $ do
                            li <- x
                            fmap concat $ sequenceA (fmap (_tactic . f) li)

(.&) :: (w -> Tactic c x) -> (x -> Tactic c y) -> (w -> Tactic c y)
(.&) = (>=>)

(.|) :: (w -> Tactic c x) -> (w -> Tactic c x) -> (w -> Tactic c x)
(.|) f g x = Tactic $ (++) <$> (_tactic $ f x) <*> (_tactic $ g x)

try :: (w -> Tactic c w) -> (w -> Tactic c w)
try f = f .| return
