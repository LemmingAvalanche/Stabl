-- TODO: make a goto word? goto any point in the stack, and continue execution from there (elements above are just pushed upwards as you execute code)

import Data.Char (isDigit)
import Data.List (words, any, all)

underflow = "stack underflow!"

pop [] = error underflow
pop (x:xs) = xs

-- See - Forth
dup [] = error underflow
dup (x:xs) = x:x:xs

-- See - Forth
-- TODO: implement instead with more general word?
swap [] = error underflow
swap [_] = error underflow
swap (x:y:xs) = y:x:xs

type Token = String

tokenize :: String -> [Token]
tokenize = words
 
interpret :: String -> Int
interpret str = let
                tokens = tokenize str 
                in interpret' (tokens,[])
                   where 
                     interpret' :: ([Token],[Int]) -> Int
                     interpret' ([],stack) = head stack
                     interpret' ((token:xs),stack)
                       | all isDigit token = let num = read token in interpret' (xs,(num:stack))
                       | all (`elem` "+-*/") token = 
                         let (¤) = case token of
                               "+" -> (+)
                               "-" -> (-)
                               "*" -> (*)
                               "/" -> div
                               _   -> error "invalid token: " ++ token
                               in eval (¤) stack where 
                                                       eval (¤) stack' = -- TODO: refactor this method into a more general one: one which takes an arbitary binary operator, a stack, and uses the two operands at the top of the stack to evaluate it (or throws stack underflow if there aren't at least two elements on the stack.)
                                                         let x = head stack'
                                                             y = head $ pop stack'
                                                             res = x ¤ y
                                                         in interpret' (xs,(res:(pop $ pop stack')))
         




 
                                                
  