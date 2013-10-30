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

tokenize :: String -> [String]
tokenize = words

-- TODO: update type to be 
interpret :: ([String],[Int]) -> Int
interpret ([],stack) = head stack
interpret ((token:xs),stack)
  | all isDigit token = let num = read token in interpret (xs,(num:stack))
  | all (`elem` "+-*/") token = 
    let (¤) = case token of
            "+" -> (+)
            "-" -> (-)
            "*" -> (*)
            "/" -> div
            _   -> error $ "invalid token: " ++ token
                   in interpret (xs, eval stack (¤))
                                                       
 -- TODO: refactor this method into a more general one: one which takes an arbitary binary operator, a stack, and uses the two operands at the top of the stack to evaluate it (or throws stack underflow if there aren't at least two elements on the stack.)

eval :: [Int] -> (Int -> Int -> Int) -> [Int]
eval stack (¤) = let x = head stack
                     y = head $ pop stack
                     res = x ¤ y
                 in res:(pop $ pop stack)
         




 
                                                
  