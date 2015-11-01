{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRankNTypes
 -XGADTs
 -XPolyKinds
 -XUndecidableInstances
#-}

{-# LANGUAGE ExistentialQuantification #-}

module FunChain where

import Data.Either

data FunChain a b = forall c f . Fun c b f => FunChain (a -> c, f)  

{-
 -- existential types don't really exist... no way to unwrap them...
_funChain :: FunChain a b -> (forall c f. (Fun c b f) => (a -> c, f))
_funChain f = case f of
                FunChain x -> x
-}

class Fun a b f | f -> a b where
    eval :: f -> a -> b
    decomp :: f -> Either (a -> b) (FunChain a b)
--type-level list, type-level list functions!

instance Fun a b (a -> b) where
    eval f x = f x
    decomp f = Left f

instance (Fun a b f, Fun b c g) => Fun a c (f, g) where
    eval (f,g) x = eval g $ eval f x
    decomp (f,g) = case decomp f of
                     Left f' -> Right $ FunChain (f', g)
                     Right (FunChain (f', rest)) -> 
                         case decomp (rest, g) of
                           Right (FunChain (h, rest')) -> Right $ FunChain (f', (h, rest'))
--Right (f', decomp (rest, g))

{-
let f = flip replicate 'a'
let f' = (((+1), f), (length, (+2)))
eval f' 5
fc = decomp f'
let (Right fc2) = fc
let FunChain (g, rest) = fc2 -- my brain just exploded
let g = case fc2 of {FunChain (g', rest) -> g'}
--    Couldn't match expected type `t' with actual type `Int -> c'
--      because type variable `c' would escape its scope
-}
--ex. if we make them all showable, can we show all intermediate results?
