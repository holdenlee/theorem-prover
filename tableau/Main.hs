{-# LANGUAGE LambdaCase, TemplateHaskell #-}

{-# OPTIONS
    -XTemplateHaskell
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XTypeSynonymInstances
    -XFlexibleInstances
    -XQuasiQuotes
#-}

module Main where

import Control.Lens hiding (Context, (|>), contexts, set)
import Control.Monad
import Control.Monad.Free
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Language.Haskell.Interpreter
import System.IO
import Text.Printf

import CmdLineSetup
import DeductionRules
import Pointed
import Prop
import QQ
import Tableau

pathList = ["C:/Users/holden-lee/Dropbox/CS/hs/haskell-utilities",
            "C:/Users/Owner/Dropbox/CS/hs/haskell-utilities",
            "C:/Users/holden-lee/Dropbox/CS/hs/theorem-prover",
            "C:/Users/holden-lee/Dropbox/CS/hs/theorem-prover/utilities",
            "C:/Users/holden-lee/Dropbox/CS/hs/theorem-prover/prop-logic",
            "C:/Users/holden-lee/Dropbox/CS/hs/theorem-prover/tableau",
            "C:/Users/Owner/Dropbox/CS/hs/theorem-prover",
            "C:/Users/Owner/Dropbox/CS/hs/theorem-prover/utilities",
            "C:/Users/Owner/Dropbox/CS/hs/theorem-prover/prop-logic",
            "C:/Users/Owner/Dropbox/CS/hs/theorem-prover/tableau"]

langList = [TemplateHaskell,
            QuasiQuotes,
            MultiParamTypeClasses,
            FunctionalDependencies,
            TypeSynonymInstances,
            FlexibleInstances]

importsIp :: InterpreterT IO ()
importsIp =
  set [searchPath := pathList,
       languageExtensions := langList] >>
  setImportsQ [("Prelude", Nothing),
               ("Data.Map", Just "M")] >>
  loadModules ["*Prop.hs", "*QQ.hs", "*Tableau.hs", "*DeductionRules.hs"] >> 
  setTopLevelModules ["Prop", "QQ", "Tableau", "DeductionRules"]

step :: String -> Workspace -> IO Workspace
step x y = 
    do
      f <- runInterpreter $ importsIp >> interpret x (as :: Workspace -> Workspace)
      case f of
       Left err -> do
         putStrLn (show err)
         return y
       Right f' -> do
         let s = (f' y)
         putStrLn (show s)
         return s

main = until_ (=="quit") step (readPrompt "> ") point

