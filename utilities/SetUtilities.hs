module SetUtilities where

import Utilities
import Data.Set as S

inserts :: (Ord a) => [a] -> S.Set a -> S.Set a
inserts = foldIterate S.insert

deletes :: (Ord a) => [a] -> S.Set a -> S.Set a
deletes = foldIterate S.delete
