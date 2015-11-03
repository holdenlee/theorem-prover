{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nondet2 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor

newtype Nondet s a = Nondet {runNondet :: StateT s [] a} deriving (Functor, Applicative, Monad, MonadPlus)

getStream :: Nondet s s
getStream = Nondet $ get

--We already have (Functor m, MonadPlus m) => Alternative (StateT s m) but we need to define "many".
instance Alternative (Nondet s) where
  empty = mzero
  (<|>) = mplus
  many p@(Nondet (StateT f)) = Nondet $ StateT (\s -> if null $ f s
                                                      then [([], s)]
                                                      else runStateT (runNondet (do
                                                                                   a <- p
                                                                                   fmap (a:) (many p))) s)
                         
