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

import LeafTree
import Parser
import Prop
import Var

parseAtom :: Parser (LeafTree (WithVar String PAtom))
parseAtom = (fmap (JustA . PVar . read) (char '?' >> (many1 digit))) <|>
            (fmap Var (char '$' >> genWord)) <|>
            (fmap (JustA . PName) genWord)

parsePL = parseExpr parseAtom

--unsafe!
removeVars :: (Functor f) => f (WithVar b a) -> f a
removeVars e = fmap (\case
                        JustA a -> a) e

leafTreeToExpr :: LeafTree (WithVar String PAtom) -> Prop' (WithVar String PAtom)
leafTreeToExpr = \case
  Pure (JustA x) -> Prop' (JustA x)
  Pure (Var a) -> Prop' (Var a)
  Free [h] -> leafTreeToExpr h
  Free li ->
    if li!!0 == JustA "~" then Not (leafTreeToExpr (li!!1))
    else case li!!1 of
          "/\\" -> And (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "\\/" -> Or  (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "->"  -> Implies (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
          "<->" -> Iff (leafTreeToExpr (li!!0)) (leafTreeToExpr (li!!2))
--warning: no order of operations right now. Figure that out!

--this seems clunky. better would be to inject a string.
antiExprExp :: Prop' (WithVar String PAtom) -> Maybe (Q Exp)
antiExprExp = \case
  Pure (Var a) -> Just $ varE (mkName a)
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
            return e

--https://wiki.haskell.org/Quasiquotation
quoteExprExp :: String -> ExpQ
quoteExprExp s =  do  loc <- location
                      let pos =  (loc_filename loc,
                                 fst (loc_start loc),
                                 snd (loc_start loc))
                      expr <- parseExpr pos s
                      expv <- dataToExpQ (const Nothing `extQ` antiExprExp) expr
                      [|removeVars $expv|]
