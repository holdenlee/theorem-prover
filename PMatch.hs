{-# OPTIONS
 
 -XFlexibleInstances

#-}

module PMatch where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Free

import Utilities
import LeafTree
import Var

--A more general way to do this is to attempt to match vars both ways (need to specify which are vars and which are free), and use a typeclass HasVar instead. When things are typed this becomes more complicated. (Ex. can't unify over functions?)
--assume var names unique
{-| Find all variables in the first tree, and attempt to match them with second. Note the 2 arguments are not symmetric (as opposed to unification).-}
pmatch' :: (Eq a, Eq b) => LeafTree (WithVar b a) -> LeafTree (WithVar b a) -> Maybe [(b, LeafTree (WithVar b a))]
pmatch' t1 t2 = case t1 of
                 Pure (Var b) -> Just [(b, t2)]
                 Pure (JustA a) -> if t1==t2 then Just [] else Nothing
                 Free li1 -> case t2 of Free li2 -> fmap concat $ sequence $ zipWith pmatch' li1 li2
                                        _ -> Nothing

{-| Takes a list of pairs from pmatch', and returns a Map if each variable is only mapped to 1 tree.-}
toSubMap :: (Eq b, Eq c, Ord b) => [(b, c)] -> Maybe (M.Map b c)
toSubMap = foldl (\m (b,c) -> m >>= (\m' -> if b `M.notMember` m' || m' M.! b == c then Just $ M.insert b c m' else Nothing)) (Just M.empty)

{-| pmatch' and then toSubMap. -}
pmatch :: (Eq a, Eq b, Ord b) => [LeafTree (WithVar b a)] -> [LeafTree (WithVar b a)] -> Maybe (M.Map b (LeafTree (WithVar b a)))
pmatch = (>>= toSubMap) `c2` fmap concat `c2` sequence `c2` zipWith pmatch'

sub :: (Ord b, HasVar b c) => M.Map b (LeafTree c) -> LeafTree c -> LeafTree c
sub m tree = tree >>= (\x -> case getVar x of
                              Just v ->
                                case M.lookup v m of
                                 Just y -> y
                                 Nothing -> return x
                              Nothing -> return x)

forward :: (Eq a, Eq b, Ord b) => [LeafTree (WithVar b a)] -> [LeafTree (WithVar b a)] ->
           LeafTree (WithVar b a) -> Maybe (LeafTree (WithVar b a))
forward li1 li2 concl = fmap (flip sub concl) (pmatch li1 li2)

backward :: (Eq a, Eq b, Ord b) => [LeafTree (WithVar b a)] -> LeafTree (WithVar b a) -> LeafTree (WithVar b a) -> Maybe [(LeafTree (WithVar b a))]
backward li c1 c2 = fmap (\x -> map (sub x) li) (pmatch [c1] [c2])
