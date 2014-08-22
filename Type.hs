{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Type (Symbol, Formula, Substitution, Type, SymbolInfo (symbol, arity, bag, displayRule, quantifier , typ , rewrite, checkEq)
, SymbolLib, defaultSymbol) where
import System.Environment
import Control.Monad
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import Search
import MathParser
import qualified Data.Hashable
import qualified Data.Set as Set

type Symbol = String

type Formula = Tree Symbol 

type Substitution = Map.Map Symbol Formula

type Type = Formula --don't make a distinction on the type level.

data SymbolInfo = SymbolInfo { symbol :: Symbol
              --, name :: String
              , arity :: Int
              , bag :: Bool
              , displayRule :: String
              , quantifier :: Bool
              , typ :: Type
              , rewrite :: Formula -> Formula
              , checkEq :: Formula -> Formula -> Bool
              }

defaultSymbol:: Symbol -> SymbolInfo
defaultSymbol s = SymbolInfo s 0 False (s ++ "(" ++ "?args" ++ ")") False (Node "" []) id (\_ -> \_ -> True)

--currently, no overloading of functions allowed.
instance Eq SymbolInfo where
  s == t = (symbol s == symbol t)

instance Ord SymbolInfo where
  compare s t = compare (symbol s) (symbol t)

type SymbolLib = Map.Map Symbol SymbolInfo


--typeChecker :: Formula -> Type -> Substitution
