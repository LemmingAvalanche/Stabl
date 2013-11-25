import Data.Char (isDigit)
import Data.List (words, any, all)

import Parser

underflow = "stack underflow!"

pop [] = error underflow
pop (x:xs) = xs

-- | See - Forth
dup [] = error underflow
dup (x:xs) = x:x:xs

-- | See - Forth
-- TODO: implement instead with more general word?
swap [] = error underflow
swap [_] = error underflow
swap (x:y:xs) = y:x:xs

-- | Rotate. See - Forth
rot [] = error underflow
rot [_] = error underflow
rot [_,_] = error underflow
rot (x:y:z:xs) = z:x:y:xs

-- TODO: possible to implement using rot (or something) and the return stack?
over [] = error underflow
over [_] = error underflow
over [_,_] = error underflow
over (x:y:xs) = y:x:y:xs

tokenize :: String -> [String]
tokenize = words

-- TODO: update type to be 

-- TODO: implement checking
parseCheckAndInterpret :: String -> [Stabl]
parseCheckAndInterpret s = interpret program 
  where program = case parseStabl "" s
                  of Right pro -> pro
                     -- Left ParseError -> error "parse error!" 

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> [Stabl]
interpret s = interpret' (s,[]) 

interpret' :: ([Stabl],[Stabl]) -> [Stabl]
interpret' ([],stack) = case (head stack) of 
  Lit num    -> stack -- Correct? 
  Quotation quot' -> stack -- Correct?
  WordCall w -> error "type error!"
interpret' (Lit n : xs, stack) = interpret' (xs, Lit n : stack)
interpret' (WordCall s : xs, stack) = 
    case s of -- TODO: really hardcoded
            "add"   -> interpret' (xs, eval stack (+))
            "minus" -> interpret' (xs, eval stack (-))
            "mul"   -> interpret' (xs, eval stack (*))
            "div"   -> interpret' (xs, eval stack div)
            -- built-in stack combinators TODO: implement
            "pop"   -> interpret' (xs, pop stack)
            "dup"   -> interpret' (xs, dup stack)
            "swap"  -> interpret' (xs, swap stack)
            "rot"   -> interpret' (xs, rot stack)
            "over"  -> interpret' (xs, over stack)
            other   -> error $ "invalid word: " ++ other 
                                                       
 -- TODO: refactor this method into a more general one: one which takes an arbitary binary operator, a stack, and uses the two operands at the top of the stack to evaluate it (or throws stack underflow if there aren't at least two elements on the stack.)

eval :: [Stabl] -> (Int -> Int -> Int) -> [Stabl]
eval stack (¤) = let x = head stack
                     y = head $ pop stack
                     res = (get x) ¤ (get y) 
                       where get t = case t of 
                               WordCall s -> error $ "was String, expected num: " ++ s 
                               Lit n -> n
                 in (Lit res):(pop $ pop stack)
         




 
                                                
  