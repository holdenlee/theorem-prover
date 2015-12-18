{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module Tableau where

import Language.Haskell.TH
import qualified Data.Map as M
import Prop

import IOUtils

data Workspace = Workspace {_assms :: M.Map Int Prop, _concl :: M.Map Int Prop}
