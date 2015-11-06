{-# OPTIONS
 
 -XFlexibleInstances

#-}

module PropLogic2 where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Control.Lens hiding (Context)
import SimpleWorkspaceM hiding (w0)
import qualified Data.Tree as T

data LTree l a = Leaf a | Node l [LTree l a] deriving Eq

data Connectives = And | Or | Implies deriving (Eq, Ord)

newtype Statement a = Statement (LTree Connectives a)

instance Show (Statement String) where
  show (Statement (Leaf x)) = x
  show (Statement (Node And [x,y])) = "(" ++ (show x) ++ " /\\ " ++ (show y) ++ ")"
  show (Statement (Node Or [x,y])) = "("++ (show x) ++ " \\/ " ++ (show y) ++ ")"
  show (Statement (Node Implies [x,y])) = "("++(show x) ++ " ==> " ++ (show y) ++ ")"
  
mp' :: [Statement String] -> Maybe (Statement String)
mp' [Statement p,Statement (Node Implies [p',q])] = if p==p' then Just $ Statement q else Nothing
mp' _ = Nothing

mp p r = Node "mp" [p,r]

or1' :: Statement String -> [Statement String] -> Maybe (Statement String)
or1' (Statement s) [Statement p] = Just $ Statement (Node Or [s,p])
or1' _ _ = Nothing

--or1 s p = Node 

or2' :: Statement String -> [Statement String] -> Maybe (Statement String)
or2' (Statement s) [Statement p] = Just $ Statement (Node Or [p,s])
or2' _ _ = Nothing

and_intro' :: [Statement String] -> Maybe (Statement String)
and_intro' [Statement p,Statement q] = Just $ Statement (Node And [p,q])
and_intro' _ = Nothing

and_destr1' :: [Statement String] -> Maybe (Statement String)
and_destr1' [Statement (Node And [p, q])] = Just $ Statement p
and_destr1' _ = Nothing

and_destr2' :: [Statement String] -> Maybe (Statement String)
and_destr2' [Statement (Node And [p, q])] = Just $ Statement q
and_destr2' _ = Nothing

(==>) p q = Node Implies [p,q]
(\/) p q = Node Or [p,q]
(/\) p q = Node And [p,q]
--not, iff

w0 = Workspace (M.fromList [("1", Statement $ Leaf "A"),
                            ("2", Statement $ (Leaf "A") ==> (Leaf "B")),
                            ("3", Statement (Leaf "A" ==> (Leaf "B" ==> Leaf "C")))])
               (M.fromList [("mp", mp')])
h1 = T.Node "1" []
h2 = T.Node "2" []
h3 = T.Node "3" []

proof :: MSWorkspace (Statement String) ()
proof = do
  h4 <- define "4" (mp h1 h2)
  h5 <- define "5" (mp h1 h3)
  h6 <- define "6" (mp h4 h3)
  return ()

final = execProof proof w0
  
