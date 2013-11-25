import Test.HUnit
import Data.Either

import Parser

name = "name"

def1 = "def " ++ name ++ " {1 2 3 4 5}"
expectedDef1 = [(name, [Lit 1,Lit 2,Lit 3,Lit 4,Lit 5])]

def2 = "def " ++ name ++ " {hei 2 3 4 hade}"
expectedDef2 = [(name, [WordCall "hei", Lit 2, Lit 3, Lit 4, WordCall "hade"])]

defList1 = def1 ++ "       " ++ def2
expectedDefList1 = expectedDef1 ++ expectedDef2

defList2 = def1 ++ "   " ++ def2 ++ "  " ++ def1
expectedDefList2 = expectedDef1 ++ expectedDef2 ++ expectedDef1

-- All tests
tests = TestList [TestLabel "test1" testDef1GivesNoError, TestLabel "test2" testDef1ParsesCorrectly, TestLabel "test3" testDef1ParsesCorrectly, TestLabel "test4" testDefList1ParsesCorrectly, TestLabel "test5" testDefList2ParsesCorrectly]

testDef1GivesNoError = TestCase $ assertBool 
                      "The definition gave an error" 
                      (case (parseDecl "" def1) 
                                 of Right _ -> True
                                    Left _ -> False)
                      
testDef1ParsesCorrectly = testParseAgainstExpected def1 expectedDef1

testDef2ParsesCorrectly = testParseAgainstExpected def1 expectedDef2

testDefList1ParsesCorrectly = testParseAgainstExpected defList1 expectedDefList1

testDefList2ParsesCorrectly = testParseAgainstExpected defList2 expectedDefList2

testParseAgainstExpected str expected = TestCase $ assertEqual "def1 was not parsed correctly" (getRight $ parseDecl "" str) expected

-- | Unsafely gets the Right value. 
getRight :: Either a b -> b
getRight (Right value) = value
