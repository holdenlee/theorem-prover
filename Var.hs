{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFlexibleInstances
 -XFunctionalDependencies
 -XDeriveDataTypeable
#-}

module Var where

import Data.Maybe
import Data.Data
import Data.Typeable

newtype IVar = IVar Int deriving (Eq, Ord)
newtype Str = Str String deriving (Eq, Ord)

instance Show Str where
  show (Str s) = s

instance (Show IVar) where
    show (IVar n) = "x"++(show n)

data WithVar b a = JustA a | Var b deriving (Eq, Ord, Show, Typeable, Data)

class HasVar b x | x -> b where
  getVar :: x -> Maybe b

instance (HasVar b (WithVar b a)) where
  getVar (JustA a) = Nothing
  getVar (Var b) = Just b
