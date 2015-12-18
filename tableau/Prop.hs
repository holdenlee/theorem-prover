{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module Prop where

import Language.Haskell.TH
import Control.Monad

type PName = String

data PAtom = PName PName | PVar Int

data Prop' p = Prop' p | Implies (Prop' p) (Prop' p) | Iff (Prop' p) (Prop' p) | And (Prop' p) (Prop' p) | Or (Prop' p) (Prop' p) | Not (Prop' p) deriving Show
--alternative : Prop' p | Unary UnOp (Prop' p) | Binary BinOp (Prop' p) (Prop' p)

{-| given a constructor, get a pattern for the constructor, and the names and types of the variables. -}
constructorToPattern :: Con -> Q ([(Name, Type)], Pat)
constructorToPattern = \case
  NormalC name t ->
    do
      let n = length t
      vars <- sequence $ map newName $ replicate n "x" 
      return (zipWith (,) vars (map snd t), ConP name $ map VarP vars)

apps :: [Exp] -> Exp
apps = foldl1 AppE

constructorToExp :: [Exp] -> Con -> Q Exp
constructorToExp exps = \case
  NormalC name t -> return (apps $ (ConE name):exps)

{-
deriveFmap :: Name -> Q [Dec]
deriveFmap name = do
  -- data CName tv = ... 
  let TyConI (DataD _ cname [KindedTV tv StarT] li) = name
-}
{-
deriv :: Q [Dec]
deriv =
  let
    x = mkName "x"
    e1 = [p| Prop' $(return $ VarP x) |]
  in
   [d| fmap f y = case y of {$e1 -> Prop' $ f $(return $ VarE x)}|]
-}
inst :: Q [Dec]
inst = do
  let x = mkName "x"
  let f = mkName "f"
  --e1 <- [p| Prop' $(return $ VarP x) |]
  e1 <- [| Prop' $ $(varE f) $(varE x) |]
  let caseSplice = (LamCaseE -- \case 
                    [Match (ConP 'Prop' [VarP x]) --Prop' x ->
                     --note the single quote for the constructor, double quote for the type function
                          (NormalB e1) []])   --Prop' x 
  [d|instance Functor $(conT ''Prop') where
        fmap $(varP f) = $(return caseSplice)|]
--fmap disappears??
{- decl <- [d| fmap f y = case y of {$e1 -> Prop' $ f $(return $ VarE x)}|]
    [InstanceD []
             (AppT (ConT ''Functor) (ConT prop))
               --instance Functor Prop' where
             decl]
-}
--emptyShow name = [d|instance Show $(conT name) where show _ = ""|]
{-
    instance Functor Prop' where
      fmap f_a5du x = case x of { Prop' x -> Prop' x }

Tableau.hs:17:1:
    Type constructor Prop' used as a constructor-like thing
    In the pattern: Prop' x
    In a case alternative: Prop' x -> Prop' x
    In the expression: case x of { Prop' x -> Prop' x }

-}
{-
  let caseSplice = (CaseE (VarE x) --case x of
                         [Match (ConP 'Prop' [VarP x]) --Prop' x ->
                          --note the single quote for the constructor, double quote for the type function
                          (NormalB e1) []])   --Prop' x 
  [d|instance Functor $(conT ''Prop') where
        fmap f $(return $ VarP x) = $(return caseSplice)|]
_]
-}
