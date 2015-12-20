{-# LANGUAGE LambdaCase #-}
{-# OPTIONS
    -XTemplateHaskell
    -XQuasiQuotes
#-}

module Test2 where

import Text.Show

data X = X deriving Show

x = map (+1) [1,2,3]
