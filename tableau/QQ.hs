{-# LANGUAGE LambdaCase #-}
{-# OPTIONS
    -XTemplateHaskell
#-}

module QQ where

import Control.Monad.Free
import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String
import Prelude -- necessary for Hint.

import LeafTree
import Parser
import Prop
import Var

parseAtom :: Parser (WithVar String PAtom)
parseAtom = (fmap (JustA . PVar . read) (char '?' >> (many1 digit))) <|>
            (fmap Var (char '$' >> genWord)) <|>
            (fmap (JustA . PName) genWord)

parsePL = parseExpr parseAtom

--unsafe!
removeVars :: (Functor f) => f (WithVar b a) -> f a
removeVars e = fmap (\case
                        JustA a -> a) e

addVars :: (Functor f) => f a -> f (WithVar b a)
addVars = fmap JustA 

stripToName (Pure (JustA (PName x))) = x

leafTreeToExpr :: LeafTree (WithVar String PAtom) -> Prop' (WithVar String PAtom)
leafTreeToExpr = \case
  Pure (JustA x) -> Prop' (JustA x)
  Pure (Var a) -> Prop' (Var a)
  Free [h] -> leafTreeToExpr h
  Free li ->
    if li!!0 == Pure (JustA (PName "~")) then Not (leafTreeToExpr (li!!1))
    else case stripToName (li!!1) of
          "/\\" -> And (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "\\/" -> Or  (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "->"  -> Implies (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "<->" -> Iff (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
--warning: no order of operations right now. Figure that out!

--this seems clunky. better would be to inject a string.
antiExprExp :: Prop' (WithVar String PAtom) -> Maybe (Q Exp)
antiExprExp = \case
  Prop' (Var a) -> Just $ return $ AppE (VarE 'addVars) $ VarE (mkName a)
  _ -> Nothing
         
parseWrapper :: Monad m => (String, Int, Int) -> String -> m (Prop' (WithVar String PAtom))
parseWrapper (file, line, col) s =
  case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- parsePL
            eof
            return $ leafTreeToExpr e

--https://wiki.haskell.org/Quasiquotation
quoteExprExp :: String -> ExpQ
quoteExprExp s =  do  loc <- location
                      let pos =  (loc_filename loc,
                                 fst (loc_start loc),
                                 snd (loc_start loc))
                      expr <- parseWrapper pos s
                      let expv = dataToExpQ (const Nothing `extQ` antiExprExp) expr
                      [|removeVars $expv|]

prop :: QuasiQuoter
prop = QuasiQuoter { quoteExp = quoteExprExp,
                     quotePat = undefined,
                     quoteType = undefined,
                     quoteDec = undefined
          }

