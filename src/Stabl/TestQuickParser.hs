import Test.QuickCheck
import Parser

prop_num :: Integer -> Bool
prop_num x = either 
             (const False) 
             ([LitInt x] ==)
             (parseStabl "" (show x))
