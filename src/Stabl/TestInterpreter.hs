import Test.HUnit
import Interpreter
import Parser

import qualified Data.Map as Map

tests = TestList [ TestLabel "test1" testDef1
                 , TestLabel "test2" testDef2
                 , TestLabel "test3" testDef3
                 , TestLabel "test4" testDef4
                 , TestLabel "test5" testDef5
                 , TestLabel "test6" testDef6
                 , TestLabel "test7" testDef7
                 , TestLabel "test8" testDef8
                 , TestLabel "test9" testDef9
                 , TestLabel "test10" testDef10
                 , TestLabel "test11" testDef11
                 , TestLabel "test12" testDef12
                  ]
 
prog1 = parseCheckAndInterpret "2 dup" Map.empty
res1 = [Lit 2, Lit 2]

prog2 = parseCheckAndInterpret "4 dup mul" Map.empty
res2 = [Lit 16]

prog3 = parseCheckAndInterpret "4 2 over add add" Map.empty
res3 = [Lit 10]

prog4 = parseCheckAndInterpret "2 2 swap swap swap" Map.empty
res4 = [Lit 2, Lit 2]

prog5 = parseCheckAndInterpret "2 4 over over dup" Map.empty
res5 = [Lit 4, Lit 4, Lit 2, Lit 4, Lit 2]

prog6 = parseCheckAndInterpret "2 4 6 mul dup add over 5" Map.empty
res6 = [Lit 5, Lit 2, Lit 48, Lit 2]

-- word definitions and quotations

prog7 = parseCheckAndInterpret "[1 2 3 4 5] apply" Map.empty
res7 = [Lit 5, Lit 4, Lit 3, Lit 2, Lit 1] -- ...eller motsatt rekkefølge?
 
prog8 = parseCheckAndInterpret "[1 2 3 4 5]" Map.empty
res8 = [Quotation [Lit 1, Lit 2, Lit 3, Lit 4, Lit 5]] 

prog9 = parseCheckAndInterpret "1 2 3 [4 5 6 7 8] apply" Map.empty
res9 = [Lit 8, Lit 7, Lit 6, Lit 5, Lit 4, Lit 3, Lit 2, Lit 1]

prog10 = parseCheckAndInterpret "2 2 [add] apply" Map.empty
res10 = [Lit 4]

-- executing user defined words
mySqr = [WordCall "dup", WordCall "mul"]
words11 = Map.fromList [("mySqr", mySqr)] :: Map.Map String [Stabl]

prog11 = parseCheckAndInterpret "3 4 [dup mul] apply" Map.empty
res11 = [Lit 16, Lit 3]

prog12 = parseCheckAndInterpret "3 4 mySqr" words11
res12 = [Lit 16, Lit 3]

testDef prog res = TestCase $ assertBool 
                   ("Failure: expected: " ++ (show res) ++ ", but got: " ++ (show prog)) 
                   $ prog == res

testDef1 = testDef prog1 res1
testDef2 = testDef prog2 res2
testDef3 = testDef prog3 res3
testDef4 = testDef prog4 res4
testDef5 = testDef prog5 res5
testDef6 = testDef prog6 res6
testDef7 = testDef prog7 res7
testDef8 = testDef prog8 res8
testDef9 = testDef prog9 res9
testDef10 = testDef prog10 res10
testDef11 = testDef prog11 res11
testDef12 = testDef prog12 res12
