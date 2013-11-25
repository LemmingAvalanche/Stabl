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
swap [_] = error underflow
swap (x:y:xs) = y:x:xs

-- | Rotate. See - Forth

rot [] = error underflow
rot [_] = error underflow
rot [_,_] = error underflow
rot (x:y:z:xs) = z:x:y:xs

over [] = error underflow
over [_] = error underflow
over (x:y:xs) = y:x:y:xs

-- TODO: implement checking
parseCheckAndInterpret :: String -> [Stabl]
parseCheckAndInterpret s = getResult $ interpret program -- TODO: implement with a dict
  where program = case parseStabl "" s
                  of Right pro -> pro
                     Left _ -> error "oooopps!"
                     -- Left ParseError -> error "parse error!"

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Result Stabl
interpret s = interpret' (Stack s, [])

-- TODO: implement these in the interpreter functions
-- Data stack
newtype Stack a = Stack { getStack :: [a] }
-- Return stack
newtype Return a = Return { getReturn :: [a] }
-- Result stack
newtype Result a = Result { getResult :: [a] }


-- TODO: fix all function-calls to also use a dictionary.
interpret' :: (Stack Stabl, [Stabl]) -> Result Stabl
interpret' (Stack [], stack) = case (head stack) of 
  Lit num    -> Result stack
  Quotation quot' -> Result stack
  WordCall w -> error "type error!"
interpret' (Stack (Lit n : xs), stack) = interpret' (Stack xs, Lit n : stack)
interpret' (Stack (WordCall s : xs), stack) = 
    -- Built-in words
    case s of -- TODO: hardcoded
            "add"   -> interpret' (Stack xs, eval stack (+))
            "minus" -> interpret' (Stack xs, eval stack (-))
            "mul"   -> interpret' (Stack xs, eval stack (*))
            "div"   -> interpret' (Stack xs, eval stack div)
            -- built-in stack combinators 
            "pop"   -> interpret' (Stack xs, pop stack)
            "dup"   -> interpret' (Stack xs, dup stack)
            "swap"  -> interpret' (Stack xs, swap stack)
            "rot"   -> interpret' (Stack xs, rot stack)
            "over"  -> interpret' (Stack xs, over stack)
            -- user-defined word TODO: implement
            other   -> error $ "invalid word: " ++ other 

eval :: [Stabl] -> (Int -> Int -> Int) -> [Stabl]
eval stack (¤) = let x = head stack
                     y = head $ pop stack 
                     res = (get x) ¤ (get y) 
                       where get t = case t of 
                               WordCall s -> error $ "was String, expected num: " ++ s 
                               Lit n -> n
                 in (Lit res):(pop $ pop stack)
         




 
                                                
  