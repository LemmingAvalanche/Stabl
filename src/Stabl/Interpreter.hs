import Data.Char (isDigit)
import Data.List (words, any, all)
import qualified Data.Map as Map

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
swap [_] = error underflowx
swap (x:y:xs) = y:x:xs

-- | Rotate. See - Forth

rot [] = error underflow
rot [_] = error underflow
rot [_,_] = error underflow
rot (x:y:z:xs) = z:x:y:xs

-- TODO: implement checking
parseCheckAndInterpret :: String -> Map.Map Word Quot -> [Stabl]
parseCheckAndInterpret s dict = interpret program
  where program = case parseStabl "" s
                  of Right pro -> pro
                     -- Left ParseError -> error "parse error!" 

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> [Stabl]
interpret s = interpret' (s, [])

-- TODO: implement these in the interpreter functions
-- Data stack
newtype Stack a = Stack [a]
-- Return stack
newtype Return a = Return [a]
-- Result stack
newtype Result a = Result [a]


-- TODO: fix all function-calls to also use a dictionary.
interpret' :: ([Stabl], [Stabl]) -> [Stabl]
interpret' ([], stack) = case (head stack) of 
  Lit num    -> stack
  Quotation quot' -> stack
  WordCall w -> error "type error!"
interpret' (Lit n : xs, stack) = interpret' (xs, Lit n : stack)
interpret' (WordCall s : xs, stack) = 
    -- Built-in words
    case s of -- TODO: hardcoded
            "add"   -> interpret' (xs, eval stack (+))
            "minus" -> interpret' (xs, eval stack (-))
            "mul"   -> interpret' (xs, eval stack (*))
            "div"   -> interpret' (xs, eval stack div)
            -- built-in stack combinators 
            "pop"   -> interpret' (xs, pop stack)
            "dup"   -> interpret' (xs, dup stack)
            "swap"  -> interpret' (xs, swap stack)
            "rot"   -> interpret' (xs, rot stack)
            "over"  -> interpret' (xs, over stack)
            -- user-defined word TODO: implement
            other   -> error $ "invalid word: " ++ other 

eval :: [Stabl] -> (Int -> Int -> Int) -> [Stabl]
eval stack (¤) = let x = head stack       -- first element of the stack
                     y = head $ pop stack -- second element of the stack
                     res = (get x) ¤ (get y) 
                       where get t = case t of 
                               WordCall s -> error $ "was String, expected num: " ++ s 
                               Lit n -> n
                 in (Lit res):(pop $ pop stack)
         




 
                                                
  