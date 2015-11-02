{-# OPTIONS
 
 -XFlexibleInstances

#-}

module Pointed where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

class Pointed a where
  point :: a

instance Pointed (M.Map a b) where
  point = M.empty

instance Pointed [a] where
  point = []

instance Pointed (S.Set a) where
  point = S.empty

instance Pointed (a -> a) where
  point = id

instance (Pointed a, Pointed b) => Pointed (a,b) where
  point = (point, point)
