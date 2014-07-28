import Test.QuickCheck

import Parser
import Interpreter

-- Help

check expected = either
                 (const False)
                 (==expected)

-- Arithmetic

helper_arith1 :: Integer -> Integer -> (Integer -> Integer -> Integer) -> String -> Bool
helper_arith1 x y op opString = let expr = show x ++ " " ++ show y ++ " " ++ opString 
                                    expected = LitInt $ x `op` y
                                    actual = fmap head $ parseAndInterpret expr 
                                in check expected actual

prop_arith_add x y = helper_arith1 x y (+) "+"
prop_arith_minus x y = helper_arith1 x y (-) "-"
prop_arith_mul x y = helper_arith1 x y (*) "*"
prop_arith_div x y = if y /= 0 
                     then helper_arith1 x y div "/"
                     else True 

-- Combinators

prop_pop1 :: Integer -> Bool
prop_pop1 x = let expr = show x ++ _spop
                  expected = []
                  actual = parseAndInterpret expr
              in check expected actual

-- 'dup pop' is identity
prop_dup_pop1 :: Integer -> Bool
prop_dup_pop1 x = let expr = show x ++ _sdup ++ _spop
                      expected = [LitInt x]
                      actual = parseAndInterpret expr
                  in check expected actual



