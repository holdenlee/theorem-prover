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

module DTactic where
import Control.Monad
import Control.Monad.Reader
import Data.Traversable
import Control.Monad.Trans.RWS.Lazy --or Strict?
import Control.Monad.Trans.Maybe
import Data.Monoid
import Control.Lens
import Control.Applicative

import Tactic
import Utilities

--s is the workspace. Note the Maybe is innermost.
type ProofState r w s a = MaybeT (RWS r w s) a

--(Functor m, Monad m) => Alternative (MaybeT m)

type Tactic' r w s a b = a -> ProofState r w s b
type Tactic r w s a = Tactic' r w s a a

proofState :: (r -> s -> (Maybe a, s, w)) -> ProofState r w s a
proofState f = MaybeT $ rws f

runProofState :: (Monoid w) => (ProofState r w s a) -> (r -> s -> (Maybe a, s, w))
runProofState = runRWS . runMaybeT

--eval context workspace beginState
evalProofState :: (Monoid w) => r -> s -> a -> Tactic' r w s a b -> (Maybe b, s, w)
evalProofState r s a t = runProofState (t a) r s

--assume techniques always check for doneness.
  {-
repeatUntilDone' :: (HasStatus w) => (w -> [(w,l)]) -> (w -> [(w,l)])
repeatUntilDone' f w =
  case f w of
   [] -> []
   (hw,hl):rest -> if succeeded hw then (hw,hl):(repeatUntilDone' (f w)
-}
