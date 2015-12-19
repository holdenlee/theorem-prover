{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}
module Tableau where

import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Control.Lens hiding (Context)

import CmdLineSetup
import Prop
import IOUtils

{-| A context is a list of assumptions (and things shown) and a conclusion to try to prove.-}
data Context = Context {_assms :: S.Set Int, _concl :: Int}

{-| A workspace is a list of statements with partial proofs, contexts, the current context (which has focus), the unproven contexts, and a supply of fresh numbers. -}
data Workspace = Workspace {_statements :: M.Map Int Prop, _proofs :: M.Map Int Prop, _contexts :: [Context], _cur :: Int, _unproven :: S.Set Int, _num :: Int}

makeLenses ''Context
makeLenses ''Workspace

switchContext :: Int -> Workspace -> Workspace
switchContext n w = if (n>=0 && n < length (_contexts w))
                    then w & cur .~ n
                    else w

-- apply' :: 



