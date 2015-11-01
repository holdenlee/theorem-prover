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

--s is the workspace. Note the Maybe is innermost.
type ProofState r w s a = MaybeT (RWS r w s) a

type Tactic' r w s a b = a -> ProofState r w s b
type Tactic r w s a = Tactic' r w s a a

(.&) :: (Monoid w) => (Tactic' r w s a b) -> (Tactic' r w s b c) -> (Tactic' r w s a c)
(.&) = (>=>)

(.|) :: (Monoid w) => (Tactic' r w s a b) -> (Tactic' r w s a b) -> (Tactic' r w s a b)
(.|) f g x = MaybeT $ (<|>) <$> (runMaybeT $ f x) <*> (runMaybeT $ g x)

try :: (Monoid w) => (Tactic r w s a) -> (Tactic r w s a)
try f = f .| return

proofState :: (r -> s -> (Maybe a, s, w)) -> ProofState r w s a
proofState f = MaybeT $ rws f

runProofState :: (Monoid w) => (ProofState r w s a) -> (r -> s -> (Maybe a, s, w))
runProofState = runRWS . runMaybeT

--eval context workspace beginState
eval :: (Monoid w) => r -> s -> a -> Tactic' r w s a b -> (Maybe b, s, w)
eval r s a t = runProofState (t a) r s

repeatT :: (Monoid w) => Int -> (Tactic r w s a) -> (Tactic r w s a)
repeatT n f = foldl1 (.&) $ replicate n f

--assume techniques always check for doneness.
  {-
repeatUntilDone' :: (HasStatus w) => (w -> [(w,l)]) -> (w -> [(w,l)])
repeatUntilDone' f w =
  case f w of
   [] -> []
   (hw,hl):rest -> if succeeded hw then (hw,hl):(repeatUntilDone' (f w)
-}
