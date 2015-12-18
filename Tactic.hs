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

{-|
  A ProofState has a log w, a context r, and a (nondeterministic) state s. (It allows searching over the state s.)
-}
type ProofState w r s a = WriterT w (ReaderT r (Nondet s)) a

{-| A transformation of ProofState: use >>= to compose. -}
type Tactic' w r s a b = a -> ProofState w r s b
type Tactic w r s a = Tactic' w r s a a

{-| Build a ProofState by a function given a context r and the current state s, that gives a list of ((result, logging), state).-}
proofState :: (r -> s -> [((a, w), s)]) -> ProofState w r s a
proofState = WriterT . ReaderT . (Nondet .)

runProofState :: (Monoid w) => ProofState w r s a -> r -> s -> [((a, w), s)]
runProofState = (runNondet .) . runReaderT . runWriterT

evalProofState :: (Monoid w) => ProofState w r s a -> r -> s -> (w, s)
evalProofState = (\((x,y),z) -> (y,z)) `c3` (!!0) `c3` runProofState
