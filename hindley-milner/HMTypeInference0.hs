module HMTypeInference where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Control.Lens hiding (Context)

newtype IVar = IVar Int deriving (Eq, Ord)

instance (Show IVar) where
    show (IVar n) = "x"++(show n)

data WithVar b a = JustA a | Var b deriving (Eq, Ord)

instance (Show a, Show b) => Show (WithVar b a) where
    show (JustA x) = show x
    show (Var y) = show y

data Expr' a b = Apply [Expr' a b] | Fun b (Expr' a b) | LetIn b (Expr' a b) (Expr' a b) | JustE a deriving (Eq, Ord)

instance (Show a, Show b) => Show (Expr' a b) where
    show (Apply li) = "(" ++ (intercalate " " $ map show li) ++ ")"
    show (Fun x f) = "(\\"++(show f)++" -> "++(show f)
    show (LetIn x e f) = "(let "++(show x)++ "=" ++ (show e) ++ " in " ++ (show f)
    show (JustE x) = show x

type Expr a b = Expr' (WithVar b a) b

(|->) = Fun
let_ = LetIn
--in_ = ($)

--annoying distinction - maybe combine them together at cost of some not making sense?
data Monotype c = JustMT c | ApplyT String [Monotype c] deriving (Eq, Ord)
data Polytype c = JustPT (Monotype c) | Forall c (Polytype c) deriving (Eq, Ord)
--data Type b c = MT (Monotype b c) | PT (Polytype b c)

instance Functor Monotype where
    fmap f (JustMT x) = JustMT (f x)
    fmap f (ApplyT str li) = ApplyT str (map (fmap f) li)

instance Functor Polytype where
    fmap f (JustPT y) = JustPT (fmap f y)
    fmap f (Forall x y) = Forall (f x) (fmap f y)

{-
class Type t where
    free' :: t c -> S.Set c-}

--data Type b c = ApplyT Arrow (Type b c) (Type b c) | JustT c | Forall b (Type b c)

instance (Show c) => Show (Monotype c) where
    show (JustMT c) = show c
    show (ApplyT b c) = if b=="->"
                       then "("++(show (c!!0))++" -> "++(show (c!!1))++")"
                       else "("++b++" "++(intercalate " " $ map show c)++")"

instance (Show c) => Show (Polytype c) where
    show (JustPT x) = show x
    show (Forall alpha pt) = "(\\/"++(show alpha)++ ". "++(show pt)++")"

{-
instance Type Monotype where
    free' (JustMT c) = S.singleton c
    free' (ApplyT x li) = S.unions $ map free' li

instance Type Polytype where
    free' (JustPT x) = free' x
    free' (Forall alpha si) = S.delete alpha $ free' si 
-}

freeM :: (Ord c) => Monotype c -> S.Set c
freeM (JustMT c) = S.singleton c
freeM (ApplyT x li) = S.unions $ map freeM li

freeP :: (Ord c) => Polytype c -> S.Set c
freeP (JustPT x) = freeM x
freeP (Forall alpha si) = S.delete alpha $ freeP si 

{-
instance (Show b, Show c) => Show (Type b c) where
    show (MT x) = show x
    show (PT y) = show y
-}

{-
instance (Show b, Show c) => Show (Type c) where
    show (Arrow a b) = "("++(show a)++" -> "++(show b)++")"
    show (JustT c) = show c
    show (Forall x f) = "(\\/"++(show x)++". "++(show f)++ ")"
-}

(-->) x y = ApplyT "->" [x,y]

data Typing a b c = IsType (Expr a b) (Polytype c) deriving (Eq, Ord)

instance (Show a, Show b, Show c) => Show (Typing a b c) where
    show (IsType exp tau) = (show exp) ++ " : " ++ (show tau)

(-::) :: Expr a b -> Polytype c -> Typing a b c
(-::) = IsType

data Predicate a b c = Spec (Polytype c) (Polytype c) | Judgment (S.Set (Typing a b c)) (Typing a b c)

(<<=) = Spec

instance (Show a, Show b, Show c) => Show (Predicate a b c) where
    show (Spec e1 e2) = (show e1) ++ " <= " ++ (show e2)
    show (Judgment s t) = intercalate "," (map show (S.toList s)) ++ " |- " ++ (show t)

(|-) = Judgment

type Context a b c = S.Set (Typing a b c)

var :: (Ord a, Ord b, Ord c) => Typing a b c -> Context a b c -> Maybe (Predicate a b c)
var typ gamma = if typ `S.member` gamma then Just (gamma |- typ) else Nothing

app :: (Eq a, Eq b, Eq c) => Predicate a b c -> Predicate a b c -> Maybe (Predicate a b c)
app (Judgment gamma0 (IsType e0 (JustPT (ApplyT "->" [tau,tau'])))) (Judgment gamma1 (IsType e1 (JustPT tau1))) = 
    if gamma0==gamma1 && tau == tau1 
    then Just (gamma0 |- ((Apply [e0,e1]) -:: JustPT tau')) 
    else Nothing
app _ _ = Nothing

abs :: (Ord a, Ord b, Ord c) => Typing a b c -> Predicate a b c -> Maybe (Predicate a b c)
abs typ@(IsType (JustE (Var x')) (JustPT tau)) (Judgment gamma (IsType e (JustPT tau'))) = 
    if typ `S.member` gamma 
    then Just ((S.delete typ gamma) |- ((x' |-> e) -:: JustPT (tau --> tau')))
    else Nothing
abs _ _ = Nothing

let' :: (Ord a, Ord b, Ord c) => b -> Predicate a b c -> Predicate a b c -> Maybe (Predicate a b c)
let' x (Judgment gamma (IsType e0 si)) (Judgment gamma' (IsType e1 tau)) = 
   if (IsType (JustE (Var x)) si) `S.member` gamma' && (S.delete (IsType (JustE (Var x)) si) gamma' == gamma)
   then Just (gamma |- ((let_ x e0 e1) -:: tau))
   else Nothing
let' _ _ _ = Nothing

inst :: (Eq c) => Predicate a b c -> Predicate a b c -> Maybe (Predicate a b c)
inst (Judgment gamma (IsType e si')) (Spec si1' si) = 
    if si' == si1'
    then Just (gamma |- (e -:: si))
    else Nothing
inst _ _ = Nothing

{-
free' :: Expr a b -> S.Set b
free' (JustE (Var x)) = S.singleton x
free' (Apply li) = S.unions (map free' li)
free' (LetIn x e f) = S.union (free' f) (free' e) & delete x
free' (Forall a si) = delete a $ free' si -}

free :: (Ord c) => Context a b c -> S.Set c
free gamma = S.unions $ map (\(IsType x si) -> freeP si) $ S.toList gamma

gen :: (Ord c) => c -> Predicate a b c -> Maybe (Predicate a b c)
gen alpha (Judgment gamma (IsType e sigma)) = 
    if alpha `S.member` (free gamma)
    then Just (gamma |- (e -:: (Forall alpha sigma)))
    else Nothing
gen _ _ = Nothing

--data Workspace = 
--deduce :: Maybe x -> Workspace -> (x, Workspace)

mapToF :: (Ord a) => M.Map a a -> (a -> a)
mapToF m x = case M.lookup x m of
               Nothing -> x
               Just y -> y

subst :: (Ord c) => M.Map c c -> Polytype c -> Polytype c
subst m (JustPT x) = JustPT (fmap (mapToF m) x)
subst m (Forall x y) = 
    case M.lookup x m of 
      Nothing -> Forall x (subst m y)
      Just z -> subst m y

x_ = IVar
a_ = IVar

gamma_0 :: Context String IVar IVar
gamma_0 = (S.fromList [(JustE (JustA "id")) -:: (Forall (x_ 1) (JustPT (JustMT (x_ 1) --> (JustMT (x_ 1))))),
                   (JustE (JustA "n")) -:: (JustPT (ApplyT "int" []))])

eq_spec :: Predicate String IVar IVar
eq_spec = (Forall (a_ 1) (JustPT (JustMT (x_ 1) --> (JustMT (x_ 1))))) <<= JustPT ((ApplyT "int" []) --> (ApplyT "int" []))

proof :: Maybe (Predicate String IVar IVar)
proof = do
  eq1 <- var ((JustE (JustA "id")) -:: (Forall (x_ 1) (JustPT (JustMT (x_ 1) --> (JustMT (x_ 1)))))) gamma_0
  eq2 <- inst eq1 eq_spec
  eq3 <- var ((JustE (JustA "n")) -:: (JustPT (ApplyT "int" []))) gamma_0
  eq4 <- app eq2 eq3
  return eq4
