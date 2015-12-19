{-# LANGUAGE LambdaCase, ViewPatterns #-}

{-# OPTIONS
 
 -XFlexibleInstances
#-}

--FIX!
module PMatchT where

import Control.Monad
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M

import Prop
import Utilities

sequenceMonoid :: (Monad m, Traversable t, Monoid a) => t (m a) -> m a
sequenceMonoid = fmap (foldl1 (<>)) . sequence

-- (a -> Either v b) -> (v -> Prop' b) -> Prop' a -> Prop' b
{-| Find all variables in the first tree, and attempt to match them with second. Note the 2 arguments are not symmetric (as opposed to unification).-}
pmatch' :: Prop -> Prop -> Maybe [(Int, Prop)] 
pmatch' t1 t2 = case (t1, t2) of
                 (Prop' (PVar b), _) -> Just [(b, t2)]
                 (Prop' (PName a), _) -> if t1==t2 then Just [] else Nothing
                 (Implies x1 x2, Implies y1 y2) -> (++) <$> (pmatch' x1 y1) <*> (pmatch' x2 y2)
                 (Iff x1 x2, Iff y1 y2) -> (++) <$> (pmatch' x1 y1) <*> (pmatch' x2 y2)
                 (And x1 x2, And y1 y2) -> (++) <$> (pmatch' x1 y1) <*> (pmatch' x2 y2)
                 (Not x1, Not y1) -> pmatch' x1 y1
                 _ -> Nothing

{-| Takes a list of pairs from pmatch', and returns a Map if each variable is only mapped to 1 tree.-}
toSubMap :: (Eq b, Eq c, Ord b) => [(b, c)] -> Maybe (M.Map b c)
toSubMap = foldl (\m (b,c) -> m >>= (\m' -> if b `M.notMember` m' || m' M.! b == c then Just $ M.insert b c m' else Nothing)) (Just M.empty)

{-| pmatch' and then toSubMap. -}
pmatch :: [(Int, Prop)] -> [Prop] -> [Prop] -> Maybe (M.Map Int Prop)
pmatch s = (>>= (toSubMap . (s++))) `c2` fmap concat `c2` sequence `c2` zipWith pmatch'

getVar :: PAtom -> Maybe Int
getVar = \case 
           PVar i -> Just i
           _ -> Nothing

sub :: M.Map Int Prop -> Prop -> Prop
sub m tree = tree >>= (\x -> case getVar x of
                              Just v ->
                                case M.lookup v m of
                                 Just y -> y
                                 Nothing -> return x
                              Nothing -> return x)

{-| Given
* initial substitution
* assumption patterns, ex. [?1 -> ?2, ?1]
* assumptions to match against, ex. [P -> Q, P]
* a conclusion, ex. ?2,
returns maybe the substitution and the conclusion
* Just (fromList [(1, P), (2, Q)], Q)
-}
forward :: [(Int, Prop)] -> [Prop] -> [Prop] -> Prop -> Maybe (M.Map Int Prop, Prop)
forward s li1 li2 concl = fmap (appendFun $ flip sub concl) (pmatch s li1 li2)

backward :: [(Int, Prop)] -> [Prop] -> Prop -> Prop -> Maybe (M.Map Int Prop, [Prop])
backward s li c1 c2 = fmap (appendFun $ \x -> map (sub x) li) (pmatch s [c1] [c2])

