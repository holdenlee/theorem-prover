{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module TemplateUtils where

import Language.Haskell.TH
import Control.Monad
import Control.Lens

{-| given a constructor, get a pattern for the constructor, and the names and types of the variables. -}
constructorToPattern :: Con -> Q ([(Name, Type)], Pat, Name)
constructorToPattern = \case
  NormalC name t ->
    do
      let n = length t
      vars <- mapM newName $ replicate n "x" 
      return (zipWith (,) vars (map snd t), ConP name $ map VarP vars, name)

apps :: [Exp] -> Exp
apps = foldl1 AppE

conNameToExp :: [Exp] -> Name -> Q Exp
conNameToExp exps name = return (apps $ (ConE name):exps)

--only works with 1 type variable right now
deriveFmap :: Name -> Q [Dec]
deriveFmap name = do
  f <- newName "f"
  info <- reify name
  -- data DataName tv = ... 
  let TyConI (DataD _ dataName [KindedTV tv StarT] li _) = info
  conList <- mapM constructorToPattern li
  let mapAccordingToType = (\(x, ty) ->
        if ty == VarT tv then AppE (VarE f) (VarE x)
        else if ty == AppT (ConT name) (VarT tv) then AppE (AppE (VarE 'fmap) (VarE f)) (VarE x)
                                                     else VarE x)
--                                                     :: (Name, Type) -> Exp
  let conListWithExprs = map (\z -> z & _1 %~ (map mapAccordingToType)) conList
  let exprPatToMatch (exprs, pat, conName) = do
        e1 <- conNameToExp exprs conName                                  
        return $ Match pat (NormalB e1) []
  matches <- mapM exprPatToMatch conListWithExprs
  [d|instance Functor $(conT dataName) where
        fmap $(varP f) = $(return $ LamCaseE matches)|]
          
