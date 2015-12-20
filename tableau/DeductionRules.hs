{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

{-# OPTIONS
    -XTemplateHaskell
    -XDeriveDataTypeable
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XTypeSynonymInstances
    -XFlexibleInstances
    -XQuasiQuotes
#-}

module DeductionRules where

import Language.Haskell.TH
import Control.Lens
import Control.Monad
import Prelude -- necessary for Hint.

import Prop
import QQ

{- {_deductionRuleAssms :: [Prop],
    _deductionRuleConcl :: Prop,
    _deductionRuleArgs :: Int,
    _deductionRuleName :: String
}-}

match = DeductionRule [[prop|(?1)|]]
                      [prop|(?1)|]
                      1
                      "id"

mp = DeductionRule [[prop|(?1)|], [prop|(?1 -> ?2)|]]
                   [prop|(?2)|]
                   2
                   "mp"

andDestrL = DeductionRule [[prop|(?1 /\ ?2)|]]
                          [prop|(?1)|]
                          2
                          "andDestrL"

andDestrR = DeductionRule [[prop|(?1 /\ ?2)|]]
                          [prop|(?2)|]
                          2
                          "andDestrR"

andIntro = DeductionRule [[prop|(?1)|], [prop|(?2)|]]
                         [prop|(?1 /\ ?2)|]
                         2
                         "andIntro"

orIntroL = DeductionRule [[prop|(?1)|]]
                         [prop|(?1 \/ ?2)|]
                         2
                         "orIntroL"

orIntroR = DeductionRule [[prop|(?2)|]]
                         [prop|(?1 \/ ?2)|]
                         2
                         "orIntroR"

orDestr = DeductionRule [[prop|(?1 \/ ?2)|], [prop|(?1 -> ?3)|], [prop|(?2 -> ?3)|]]
                        [prop|(?3)|]
                        3
                        "orDestr"

notIntro = DeductionRule [[prop|(?1 -> ?2)|], [prop|(?1 -> (~ ?2))|]]
                         [prop|(~ ?1)|]
                         2
                         "notIntro"

notDestr = DeductionRule [[prop|(~ ?1)|], [prop|(?1)|]]
                         [prop|(?2)|]
                         2
                         "notDestr"

doubleNeg = DeductionRule [[prop|(~ (~ ?1))|]]
                     [prop|(?1)|]
                     1
                     "doubleNeg"

iffIntro = DeductionRule [[prop|((?1 -> ?2) /\ (?2 -> ?1))|]]
                          [prop|(?1 <-> ?2)|]
                          2
                          "iffIntro"

iffDestr = DeductionRule [[prop|(?1 <-> ?2)|]]
                          [prop|((?1 -> ?2) /\ (?2 -> ?1))|]
                          2
                          "iffIntro"


                         
