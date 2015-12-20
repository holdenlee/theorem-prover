module IOUtils where

import Control.Monad
import Prelude -- necessary for Hint.

printList :: (Show a) => [a] -> IO ()
printList li = mapM_  (putStrLn . show) li
