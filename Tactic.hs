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

--Reader is commutative
--context, log, and workspace
type Tactic c l w = WriterT l (ListT (Reader c)) w

instance (Monad m) => Monoid (ListT m a) where
    mempty = ListT $ return []
    mappend t1 t2 = ListT $ (++) <$> (runListT t1) <*> (runListT t2)

{-
instance (Monoid l) => Monoid (Tactic c l w) where
  mempty = runWriterT-}

instance (Monad m, Monoid l, Monoid (m (w, l))) => Monoid (WriterT l m w) where
  mempty = WriterT $ mempty
  mappend t1 t2 = WriterT $ (runWriterT t1) <> (runWriterT t2)

(.&) :: (Monoid l) => (w -> Tactic c l x) -> (x -> Tactic c l y) -> (w -> Tactic c l y)
(.&) = (>=>)

(.|) :: (Monoid l) => (w -> Tactic c l x) -> (w -> Tactic c l x) -> (w -> Tactic c l x)
(.|) f g x = (f x) <> (g x)

try :: (Monoid l) => (w -> Tactic c l w) -> (w -> Tactic c l w)
try f = f .| return

makeTactic :: (Monoid l) => (c -> w -> [(w, l)]) -> w -> Tactic c l w
makeTactic f w = do
  c <- ask --ask for the context
  WriterT $ ListT $ return (f c w)

runTactic :: (Monoid l) => (w -> Tactic c l w) -> (c -> w -> [(w, l)])
runTactic t c w = c & (runReader $ runListT $ runWriterT (t w))

--write in terms of runTactic?
eval :: c -> Tactic c l w -> (w, l)
eval g s = (g & (runReader $ runListT $ runWriterT s))!!0

repeatT :: (Monoid l) => Int -> (w -> Tactic c l w) -> (w -> Tactic c l w)
repeatT n f = foldl1 (.&) $ replicate n f

class HasStatus a where
  succeeded :: a -> Bool

--assume techniques always check for doneness.
  {-
repeatUntilDone' :: (HasStatus w) => (w -> [(w,l)]) -> (w -> [(w,l)])
repeatUntilDone' f w =
  case f w of
   [] -> []
   (hw,hl):rest -> if succeeded hw then (hw,hl):(repeatUntilDone' (f w)
-}
