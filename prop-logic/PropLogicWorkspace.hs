{-# LANGUAGE ViewPatterns #-}


module PropLogicWorkspace where

import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as M
import Text.Parsec
--import Text.Parsec.String

import PropLogic
import Workspace
import Tactic
import Utilities
import PMatch
import LeafTree
import Var

{-
type PAtom = WithVar IVar Str

newtype Statement = Statement (LeafTree PAtom)
-}

{-type ProofState w r s a = WriterT w (ReaderT r (Nondet s)) a-}

type PLWorkspace = DAGWorkspace Statement BoxContext Statement

data Axiom = Axiom [Statement] Statement

unS :: Statement -> LeafTree PAtom
unS (Statement s) = s

p :: String -> Statement
p = Statement . fromRight . parse parsePL ""

--need to FAIL if have free variables
mp = Axiom [p"(-> ?0 ?1)", p"?0"] (p"?1")
or_destr1 = Axiom [p"?0"] (p"(\\/ ?0 ?1)")
or_destr2 = Axiom [p"?1"] (p"(\\/ ?0 ?1)")

data WContext = WContext {_library :: M.Map String Axiom}
                             -- _symbolLib :: SymbolLib

forwardReason :: String -> Bool -> [G.Node] -> ProofState String WContext PLWorkspace G.Node
--(c -> [form] -> Maybe (form, hc, str)) -> Bool -> [G.Node] -> c -> DAGWorkspace form ctxt hc -> [((G.Node, str), DAGWorkspace form ctxt hc)]
forwardReason name destr hyps = proofState (forwardReason' f destr hyps)
  where
    f c fs = do
      let Axiom (map unS -> hyp) (unS -> concl) = (_library c) M.! name
      concl' <- forward hyp (map unS fs) concl
      --need to update this to give substitution!
      return (Statement concl', Statement $ node [leaf $ JustA $ Str name], "")
