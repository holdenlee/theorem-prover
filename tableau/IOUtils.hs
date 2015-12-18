module IOUtils where

import Control.Monad

printList :: (Show a) => [a] -> IO ()
printList li = mapM_  (putStrLn . show) li
