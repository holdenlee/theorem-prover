{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XUndecidableInstances
#-}

{-# LANGUAGE ExistentialQuantification #-}

module DFS where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Bifunctor

class DFS a s b | b -> a where 
    dfs :: b -> MaybeT (State s) a
--State s (Maybe a)
--s -> (Maybe a, s)

--[s->(s,a)]
--[s -> (s, [a])]
--s -> (s,[a])
instance DFS a s (State s [a]) where
    dfs st = MaybeT $ do
               li <- st -- ::[a]
               return $ case li of
                          [] -> Nothing
                          h:rest -> Just h 


--instance (DFS a s m, DFS b s n) => DFS b s (m, a -> n) where

instance (DFS b s n) => DFS b s (State s [a], a -> n) where
    dfs (s, f) = MaybeT $ do
                   li <- s
                   case li of
                     [] -> return Nothing
                     h:rest -> do
                             maybeA <- runMaybeT $ dfs (f h)
                             case maybeA of
                               x@(Just _) -> return x
                               Nothing -> runMaybeT $ dfs ((s >> return rest), f)

data DFSable a s = forall m . DFS a s m => DFSable a s m

--instance Monad DFSable

{-
--list sequencing?
newtype DFS s a = DFS (State s [a])

_state (DFS x) = x

instance Functor (DFS s) where
    fmap f (DFS x) = DFS (fmap (map f) x)

instance Applicative (DFS s) where
    pure = DFS . return . return
    (<*>) = ap

seqDFS :: ([a], s) -> State s [b] -> 
seqDFS (li, s) = 

instance Monad (DFS s) where
    (DFS x) >>= f = DFS $ do
                         li <- x
                         s <- get
                         case map (_state . f) li of
                           g:rest -> 
                               case g s of
                                 (s', []) ->  
-}
{-
newtype DFS s a = DFS (s -> [(a,s)])

instance Functor (DFS s) where
    fmap f (DFS g) = DFS $ (fmap (first f) . g)

instance Applicative (DFS s) where
    pure x = DFS $ (return . (x,))
    (<*>) = ap

instance Monad (DFS s) where
    x >>= f s = 
        let li = x s
        in case li of
             h:
-}
