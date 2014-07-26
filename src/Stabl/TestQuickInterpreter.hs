import Test.QuickCheck

import Parser
import Interpreter

helper_arith1 :: Integer -> Integer -> (Integer -> Integer -> Integer) -> String -> Bool
helper_arith1 x y op opString = let expr = show x ++ " " ++ show y ++ " " ++ opString 
                                    in let expected = x `op` y
                                           actual = parseAndInterpret expr 
                                       -- TODO : refactor
                                       in case actual of Left _ -> False
                                                         Right actual' -> LitInt expected == head actual'

prop_arith_add x y = helper_arith1 x y (+) "+"
prop_arith_minus x y = helper_arith1 x y (-) "-"
prop_arith_mul x y = helper_arith1 x y (*) "*"
prop_arith_div x y = if y /= 0 
                     then helper_arith1 x y div "/"
                     else True 
