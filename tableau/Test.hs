{-# LANGUAGE LambdaCase #-}
{-# OPTIONS
    -XTemplateHaskell
    -XQuasiQuotes
#-}

module Main where

import Control.Monad.Free
import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String

import LeafTree
import Parser
import Prop
import Var

import QQ

p = [prop|(P \/ (~ Q))|]

q = [prop|(P -> $p)|]

main = putStrLn $ show q
