import Test.HUnit
import Interpreter
import Parser

tests = TestList [ TestLabel "test1" testDef1
                 , TestLabel "test2" testDef2
                 , TestLabel "test3" testDef3
                 , TestLabel "test4" testDef4
                  ]
 
prog1 = parseCheckAndInterpret "2 dup"
res1 = [Lit 2, Lit 2]

prog2 = parseCheckAndInterpret "4 dup mul"
res2 = [Lit 16]

prog3 = parseCheckAndInterpret "4 2 over add add"
res3 = [Lit 10]

prog4 = parseCheckAndInterpret "2 2 swap swap swap"
res4 = [Lit 2, Lit 2]

testDef prog res = TestCase $ assertBool "returned wrong result" $ prog == res

testDef1 = testDef prog1 res1
testDef2 = testDef prog2 res2
testDef3 = testDef prog3 res3
testDef4 = testDef prog4 res4
                                                                  
-- A change!

-- util functions

-- | Unsafely gets the Right value. 
getRight :: Either a b -> b
getRight (Right value) = value