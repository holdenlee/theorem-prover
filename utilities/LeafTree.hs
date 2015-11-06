module LeafTree where

import Control.Monad.Free

type LeafTree = Free [] 

leaf = Pure
node = Free
