{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Runner (ExitCode (Fail, OK, Success), Runner (Runner), cont, fun, run, wrapRunner, wrapRunner1, wrapRunner2, failIf, Pointed, funToRunner, point, getLog, setLog, Runner.empty, Runner.join, joins, origJoin, Runner.or, Runner.try, Runner.many, Runner.count, joinWith, rmap, rmapm, rmap2, rmapm2, check, getCont, setCont, (.>), (.|), doNothing, funToRunner, joinD, (.>.)) where
import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
import qualified Data.Graph.Inductive as G
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Search
import Text.ParserCombinators.Parsec
import Data.Char
import Text.Parsec.Prim
import Utilities
import qualified Data.Set as Set

data ExitCode = Fail | OK | Success deriving (Eq, Show)

--simplified Runner
data Runner a b c = Runner {cont ::c
                  , fun :: (a, c , ExitCode) ->(b,  c, ExitCode)}

run::Runner a b c -> a -> (b,c,ExitCode)
run r x = (fun r) (x, cont r, OK)

class Pointed a where
  point :: a
{-
instance (Pointed c) => Monad a a c where
  return j = Runner point (\(x,y,z) -> (x,j,z))
  (>>=) r f = Runner 
-}
funToRunner:: (Pointed c) => ((a,c,ExitCode) -> (b,c,ExitCode)) -> Runner a b c
funToRunner f = Runner point f

--note Runner a b c is monadic in a. 
--like fmap
rmap:: (Pointed c) => (a->b) -> Runner a b c
rmap f = Runner point (\(x,y,z) -> (f x,y,z))
--rmap pt f = Runner pt (\(x,y,z) -> (f x,y,z))

rmapm:: (Pointed c) => (a->Maybe a) -> Runner a a c
rmapm f = Runner point (\(x,y,z) -> 
  case (f x) of
    Just x2 -> (x2,y,z)
    Nothing -> (x, y, Fail))

--warning: no reference to a.
rmap2:: (Pointed c) => (c->c) -> Runner a a c
rmap2 f = Runner point (\(x,y,z) -> (x,f y,z))

rmapm2:: (Pointed c) => (c->Maybe c) -> Runner a a c
rmapm2 f = Runner point (\(x,y,z) -> 
  case (f y) of
    Just y2 -> (x,y2,z)
    Nothing -> (x, y, Fail))

--rmap3:: a -> c -> (c->c) -> Runner a a c
--rmap3 _ pt f = Runner pt (\(x,y,z) -> (x,f y,z))

check:: (Pointed c) => (a->Bool) -> Runner a a c
check p = Runner point (\(x,y,z) -> if (p x) then (x,y,z) else (x,y,Fail))

loadIn:: Runner a b c -> a -> (a,c,ExitCode)
loadIn r x = (x, cont r, OK)
                  
wrapRunner:: (Pointed c1) => ((a1, c1) -> c -> (a, c, ExitCode)) -> ((a1, c1) -> (b,c, ExitCode) -> (b1, c1, ExitCode)) -> Runner a b c -> Runner a1 b1 c1
wrapRunner entryFun exitFun r = Runner point (\(xa1, xc1, xe1) ->
  let 
    (xa, xc, xe) = entryFun (xa1, xc1) (cont r)
    y  = (fun r) (xa,xc, xe)
  in
    exitFun (xa1,xc1) y)
    
wrapRunner1:: (Pointed c1) => (a -> (c, ExitCode) -> (c1, ExitCode)) -> Runner a b c -> Runner a b c1
wrapRunner1 exitFun r = Runner point (\(xa, xc, xe) -> 
  let 
    (ya, yc, ye) = (fun r) (xa, (cont r), OK)
    (zc, ze) = exitFun xa (yc, ye) 
  in
    (ya, zc, ze))
    
wrapRunner2:: (Pointed c1) => (c1 -> c-> c1) -> Runner a b c -> Runner a b c1
wrapRunner2 exitFun r = Runner point (\(xa, xc, xe) -> 
  let 
    (ya, yc, ye) = (fun r) (xa, (cont r), xe)
  in 
    if ye==Fail then (ya, xc, Fail) else (ya, exitFun xc yc, ye))


failIf:: (Pointed c) => ((a,c)->Bool) -> Runner a a c
failIf p = Runner point (\(x,y,z) -> if p (x,y) then (x,y,z) else (x,y,Fail))
  
getLog:: (a,b,ExitCode) -> ExitCode
getLog (x,y,z) = z

setLog:: ExitCode -> (a,b,ExitCode) -> (a,b,ExitCode)
setLog z (x,y,_)= (x,y,z)

--empty:: (Pointed c) => Runner a a c
empty:: c -> Runner a a c
empty pt = Runner pt id

--join:: Runner a b o -> Runner b c o -> Runner a c o
join:: Runner a a o -> Runner a a o -> Runner a a o
join r1 r2 = Runner (cont r1) (\x -> 
  let 
    y = (fun r1) x
  in 
    if getLog y ==Fail then y else (fun r2) y)

--join different things, with a default
joinD:: Runner a b o -> (c,Runner b c o) -> Runner a c o
joinD r1 (z,r2) = Runner (cont r1) (\x -> 
  let 
    y = (fun r1) x
    (yb,yo,ye) = y
  in 
    if ye == Fail then (z,yo,Fail) else (fun r2) y)

joinWith:: (a->[a]) -> [Runner a a c] -> Runner a a c
--input:a, sub:b
--a = tree b = sub
--start with tr. each time 
joinWith trees patts = (patts!!0){fun = \input ->
 let (x,y,z)= input
 in
  if (length (trees x) < length patts)
    then setLog (Fail) input -- too few trees to pattern match on!
    else 
      let (tree, _, _) = input
      in foldl (\(xa, xb, xc) -> \(tree, patt)-> (fun patt) (tree, xb, xc)) input (zip (trees tree) patts)}
--the input is a tree. We get a list of trees (trees input) from our input.
--Start with the given input. Every time, take the substitution, and try to pattern-match with the next tree and pattern in the list.

--restart
--join by default allows failure? this one doesn't?
origJoin:: Runner a b c -> Runner a b c -> Runner a b c
origJoin p1 p2 = p1{fun = (\input -> 
  let 
    (x,y,z) = input
    (x1,y1,z1) = (fun p1) input
  in if (z1==Fail) 
    then (x1,y1,z1)
    else (fun p2) (x,y1,z1))}

or::Runner a b o -> Runner a b o -> Runner a b o
or r1 r2 = Runner (cont r1) (\x -> 
  let 
    f = fun r1
    g = fun r2
  in
    if (getLog (f x) /= Fail)
      then 
        f x
      else 
        g x)

--try:: Runner a b o -> Runner a b o        
try:: Runner a a o -> Runner a a o        
try r = 
  let f = (fun r)
  in r{fun = (\x ->
    if (getLog (f x) /= Fail) then f x else setLog Fail x)}

many:: Runner a a o -> Runner a a o          
many r = 
  let f = (fun r)
  in
    r{fun = stopBefore (\y->getLog y == Fail) f}
    
count:: Int -> Runner a a o -> Runner a a o 
count n r = 
  let f = (fun r)
  in
    r{fun = (\x -> if n == 0 then x else (if getLog x == Fail then x else (fun (Runner.count (n-1) r)) (f x)))}

ors:: [Runner a b o] -> Runner a b o
ors li = foldl1 Runner.or li

joins:: [Runner a a o] -> Runner a a o
joins li = foldl1 Runner.join li

--(.>.)::Runner a b o -> Runner b c o -> Runner a c o
(.>)::Runner a a o -> Runner a a o -> Runner a a o
(.>) x y = Runner.join x y

(.>.)::Runner a b o -> (c, Runner b c o) -> Runner a c o
(.>.) x y = Runner.joinD x y

infixl 0 .>

(.|)::Runner a b o -> Runner a b o -> Runner a b o
(.|) x y = Runner.or x y

setA:: (Pointed c) => b -> Runner a b c
setA f = Runner point (\(x,y,z) -> (f,y,z))

getA:: (a,b,c) -> a
getA (x,y,z) = x

setCont:: c-> Runner a a c
setCont s = Runner s (\(x,y,z) -> (x, s, z))

getCont::(a,b,c)-> b
getCont (x,y,z) = y

doNothing:: (Pointed c) => Runner a a c
doNothing = rmap id
