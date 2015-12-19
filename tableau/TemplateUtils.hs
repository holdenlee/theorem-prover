{-# LANGUAGE LambdaCase #-}

{-# OPTIONS
    -XTemplateHaskell
#-}

module TemplateUtils where

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Traversable

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
deriveFunctor :: Name -> Q [Dec]
deriveFunctor name = do
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

deriveTraversable :: Name -> Q [Dec]
deriveTraversable name = do
  f <- newName "f"
  info <- reify name
  -- data DataName tv = ... 
  let TyConI (DataD _ dataName [KindedTV tv StarT] li _) = info
  conList <- mapM constructorToPattern li
  -- no support for empty constructors right now.
  let mapAccordingToType = (\(x, ty) ->
        if ty == VarT tv then AppE (VarE f) (VarE x)
        else if ty == AppT (ConT name) (VarT tv) then AppE (AppE (VarE 'traverse) (VarE f)) (VarE x)
                                                     else VarE x)
--                                                     :: (Name, Type) -> Exp
  let conListWithExprs = map (\z -> z & _1 %~ (map mapAccordingToType)) conList
  let exprPatToMatch (exprs, pat, conName) = do
        e1 <- foldl (\x y -> do {x'<-x; return $ AppE (AppE (VarE '(<*>)) x') y}) 
                    (return $ apps [(VarE '(<$>)), ConE conName, (exprs!!0)]) 
                    (tail exprs)
        return $ Match pat (NormalB e1) []
  matches <- mapM exprPatToMatch conListWithExprs
  [d|instance Foldable $(conT dataName) where
        foldMap = foldMapDefault
     instance Traversable $(conT dataName) where
        traverse $(varP f) = $(return $ LamCaseE matches)|]

                                                                  
deriveApplicative :: Name -> Name -> Q [Dec]
deriveApplicative pureCon name = do
  info <- reify name
  -- data DataName tv = ... 
  let TyConI (DataD _ dataName [KindedTV tv StarT] li _) = info
  x <- newName "x"
  [d|instance Applicative $(conT dataName) where
        pure $(varP x) = $(return $ AppE (ConE pureCon) (VarE x))
        (<*>) = ap|]

deriveMonad :: [Name] -> Name -> Q [Dec]
deriveMonad pureCons name = do
  f <- newName "f"
  info <- reify name
  -- data DataName tv = ... 
  let TyConI (DataD _ dataName [KindedTV tv StarT] li _) = info
  conList <- mapM constructorToPattern li
  let mapAccordingToType = (\(x, ty) ->
        if ty == AppT (ConT name) (VarT tv) then AppE (AppE (VarE '(>>=)) (VarE x)) (VarE f) 
                                                     else VarE x)
  let exprPatToMatch (varAndTypes, pat, conName) =
        do
          e1 <- if conName `elem` pureCons
                then return $ AppE (VarE f) (VarE $ fst $ varAndTypes!!0)
                     --only one here...
                else conNameToExp (map mapAccordingToType varAndTypes) conName
          return $ Match pat (NormalB e1) []
  matches <- mapM exprPatToMatch conList
  x <- newName "x"
  [d|instance Monad $(conT dataName) where
        (>>=) $(varP x) $(varP f) = $(return $ CaseE (VarE x) matches)|]
