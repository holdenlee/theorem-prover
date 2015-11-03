module Nondet where

import Control.Applicative
import Control.Monad
import Data.Bifunctor

newtype Nondet s a = Nondet (s -> [(a,s)])

runNondet (Nondet f) = f

instance Functor (Nondet s) where
  fmap f (Nondet g) = Nondet $ map (first f) . g

instance Applicative (Nondet s) where
  pure x = Nondet (\s -> [(x, s)])
  (<*>) = ap

getStream :: Nondet s s
getStream = Nondet (\s -> [(s, s)])

instance Alternative (Nondet s) where
  empty = Nondet (\x -> [])
  (Nondet f) <|> (Nondet g) = Nondet (\x -> (f x) ++ (g x))
  many p@(Nondet f) = Nondet (\s -> if null $ f s
                                    then [([], s)]
                                    else runNondet (do
                                      a <- p
                                      fmap (a:) (many p)) s)
                         
instance Monad (Nondet s) where
  x >>= f = Nondet $ concat . map (uncurry (runNondet . f)) . (runNondet x)
