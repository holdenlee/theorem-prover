{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Main where
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
import Data.Monoid
import qualified Data.MultiMap as MM
import Data.Dynamic

import Utilities
import Runner
import Type
import TreeParser
import MathDAG
import Pattern
import State
import Tactics
import MathSession

testFormulas :: String
testFormulas = 
  "prop(mp, forall(P, forall(Q, implies(P, implies(implies(P,Q),Q)))))\n prop(h1 , forall(P, forall(Q, implies(P, implies(Q, P)))))\n prop(h2 , forall(P, forall(Q, forall(R, implies( implies(P, implies(Q, R)), implies( implies(P, Q), implies(P, R)))))))\n prop(h3 , forall(P, forall(Q, implies(implies(not(P), not(Q)), implies(Q, P)))))"

{-
  case s of 
    "forall" -> forAll
    "implies"-> implies
    "not" -> Symbol s s 1 False "~(?1)" False "" False []
    "or" -> Symbol s s 2 False "(?1 or ?2)" False "" False []
    "and" -> Symbol s s 2 False "(?1 and ?2)" False "" False []
    _->Symbol s s (-1) False (s ++ "(" ++ "?args" ++ ")") False "" False []
-}

testLib :: SymbolLib
testLib = Map.fromList [
	("forall", (defaultSymbol "forall"){displayRule = "(\\/ ?1. ?2)"}),
	("implies", (defaultSymbol "implies"){displayRule = "(?1 => ?2)"})]

test1::IO()
test1 = do 
	let f1 = parseFormula "forall(x, forall(y, implies(P,implies(Q,R))))"
--"forall(x, forall(y, implies(P, implies(Q, implies(R)))))"
	putStrLn (show2 testLib f1)
	let (f2,c2,e2) = (Runner.run unfoldForAll) f1
	putStrLn (show2 testLib f2)
	putStrLn (show (c2,e2))
	let (f3,c3,e3) = (Runner.run unfoldAll) f1
	putStrLn (show2 testLib f3)
	putStrLn (show (c3,e3))
        let a = parseFormula "A"
        let (f4,c4,e4) = (Runner.run (Pattern.apply a)) f3
	putStrLn (show2 testLib f4)
	putStrLn (show (c4,e4))

showFResult:: (Show b, Show c) => (Formula, b, c) -> IO ()
showFResult (f, c, e) = do
            putStrLn (show2 testLib f)
            putStrLn (show (c, e))

showMSResult:: (MathSession, State, ExitCode) -> IO ()
showMSResult (ms, c, e) = do
            putStrLn (showMS ms)
            putStrLn (show ((get "message" c)::String, e))

testGraft:: IO ()
testGraft = do
        let x = matchVar "x"
        let y = matchVar "y"
        let a = matchJustSymbol "A"
        let b = matchJustSymbol "B"
        let pl= matchJustSymbol "+"
        let xx = graftPattern pl [x,x]
        let xy = graftPattern pl [x,y]
        let xa = graftPattern pl [x,a]
        let aa = graftPattern pl [a,a]
        let ab = graftPattern pl [a,b]
        let faa = parseFormula "+(A,A)"
        let fab = parseFormula "+(A,B)"
        showFResult (Runner.run xx faa) --should succeed
        showFResult (Runner.run xx fab) --should fail
        showFResult (Runner.run xy fab) --should succeed  
        showFResult (Runner.run xa fab) --should fail
        showFResult (Runner.run aa faa) --should succeed
        showFResult (Runner.run aa fab) --should fail        

testSub::IO ()
testSub = do 
        let a = parseFormula "A"
        let b = parseFormula "B"
        showFResult (fun makeSub (a, Map.empty -: Map.insert "A" b, OK))

main = do 
        sampleProof
--      test1
--	let (f3,c3,e3) = (Runner.run unfoldAll) f1
--	putStrLn (show2 testLib f3)
--	putStrLn (show (c3,e3))



testFindSubs::IO()
testFindSubs = do 
	let p = parseFormula "P"
	let q = parseFormula "Q"
	let pq = parseFormula "implies(P,Q)"
	let a = parseFormula "A"
	let b = parseFormula "B"
	let ab = parseFormula "implies(A,B)"
	
        
        putStrLn "Test 5"
	let mp = parseFormula "forall(P, forall(Q, implies(P, implies(implies(P,Q),Q))))"
	putStrLn ("Trying to substitute " ++ (show2 Map.empty p) ++ "\n" ++ (show2 Map.empty pq) ++ "\ninto theorem" ++ (show2 Map.empty mp))
	let (f5, c5, e5) = (Runner.run (findSubs [p,pq])) mp
	putStrLn (show2 testLib f5)
	putStrLn (show (c5,e5))
--this works

        putStrLn "Test 5.1"
	let mp = parseFormula "forall(P, forall(Q, implies(P, implies(implies(P,Q),Q))))"
	putStrLn ("Trying to substitute " ++ (show2 Map.empty a) ++ "\n" ++ (show2 Map.empty ab) ++ "\ninto theorem" ++ (show2 Map.empty mp))
	showFResult (Runner.run (findSubs [a,ab]) mp)
--this works

	putStrLn "Test 6"
	let (f6, c6, e6) = Runner.run (matchVar "A") p
	putStrLn (show2 testLib f6)
	putStrLn (show (c6,e6))
       --should succeed with A->P

	putStrLn "Test 7"
	let bb = parseFormula "implies(B,B)"
	let bc = parseFormula "implies(B,C)"
	let (f7, c7, e7) = Runner.run (graftPattern (matchJustSymbol "implies") [matchVar "A",matchVar "A"]) bc
	putStrLn (show2 testLib f7)
	putStrLn (show (c7,e7))
       --should fail

        putStrLn "Test 8"
        let (f8, c8, e8) = Runner.run (formulaToPattern2 ["A"] (parseFormula "implies(A,A)")) bb
        putStrLn (show2 testLib f8)
	putStrLn (show (c8,e8))
--should succeed with A -> B

        let (f9, c9, e9) = Runner.run (formulaToPattern2 ["A"] (parseFormula "implies(A,A)")) bc
        putStrLn (show2 testLib f9)
	putStrLn (show (c9,e9))
        
        --let p1 = graftPattern (just


testMD ::IO ()
testMD = do 
	let fstring = "implies(implies(A,implies(B,C)), implies(implies(A,B),implies(A,C)))"
	let f10= parseFormula fstring
	let md = initMathDAG f10
	putStrLn (showMathDAG testLib md)
	
	let (md11, c11, e11) = Runner.run (unfoldProp 1 [2,3,4,5,6,7,8,9,10]) md
	putStrLn (showMathDAG testLib md11)
	putStrLn (show (c11,e11))

	let (md12, c12, e12) = Runner.fun (forwardReason (getProp 3 md11) [4] 6) (md11, c11, e11)
	putStrLn (showMathDAG testLib md12)
	putStrLn (show (c12,e12))


testMS::IO()
testMS = do
        let fstring = "implies(implies(A,implies(B,C)), implies(implies(A,B),implies(A,C)))"
        
	let ms20 = initSession testFormulas testLib fstring
	putStrLn (showMS ms20)
       	putStrLn (showLibrary ms20)

	let msc21 = Runner.run (unfoldProp2 1 [2,3,4,5]) ms20
	showMSResult msc21

	let msc22 = fun (forwardReason2 [4,3] 6 "mp" False) msc21
	showMSResult msc22

       	let msc23 = fun (forwardReason2 [4,2] 7 "mp" False) msc22
	showMSResult msc23

       	let msc24 = fun (forwardReason2 [6,7] 5 "mp" False) msc23
	showMSResult msc24
        let (ms24,_,_)=msc24
        putStrLn (show (currentGoals ms24))

sampleProof::IO()
sampleProof = do
        let fstring = "implies(implies(A,implies(B,C)), implies(implies(A,B),implies(A,C)))"
	let start = initSession testFormulas testLib fstring
        let proof = Runner.run (unfoldProp2 1 [2,3,4,5]
                  .> forwardReason2 [4,3] 6 "mp" False
                  .> forwardReason2 [4,2] 7 "mp" False
                  .> forwardReason2 [6,7] 5 "mp" False) start
        showMSResult proof







