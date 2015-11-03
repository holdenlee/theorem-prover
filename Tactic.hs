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
import Control.Lens

import Utilities
import Nondet

type ProofState w r s a = WriterT w (ReaderT r (Nondet s)) a

type Tactic' w r s a b = a -> ProofState w r s b
type Tactic w r s a = Tactic' w r s a a

proofState :: (r -> s -> [((a, w), s)]) -> ProofState w r s a
proofState = WriterT . ReaderT . (Nondet .)

runProofState :: (Monoid w) => ProofState w r s a -> r -> s -> [((a, w), s)]
runProofState = (runNondet .) . runReaderT . runWriterT
--not: runNondet . runReaderT . runWriterT

evalProofState :: (Monoid w) => ProofState w r s a -> r -> s -> (w, s)
evalProofState = (\((x,y),z) -> (y,z)) `c3` (!!0) `c3` runProofState
