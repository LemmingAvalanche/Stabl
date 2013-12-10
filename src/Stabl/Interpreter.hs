module Interpreter
       (
         parseCheckAndInterpret
       , interpret
       , getStack
       , getResult
       , getReturn
       , apply
       ) where

import qualified Data.Map as Map

import Parser

underflow = "stack underflow!"

pop [] = error underflow
pop (x:xs) = xs

-- | See - Forth
dup [] = error underflow
dup (x:xs) = x:x:xs

-- | See - Forth
swap [] = error underflow
swap [_] = error underflow
swap (x:y:xs) = y:x:xs

-- | See - Forth
rot [] = error underflow
rot [_] = error underflow
rot [_,_] = error underflow
rot (x:y:z:xs) = z:x:y:xs

-- | See - Forth
over [] = error underflow
over [_] = error underflow
over (x:y:xs) = y:x:y:xs

apply :: [Stabl] -> Map.Map String [Stabl] -> [Stabl] -> [Stabl]
apply stack dict quot = getResult $ interpret ((reverse quot) ++ stack) dict

-- TODO: implement checking
parseCheckAndInterpret :: String -> Map.Map String [Stabl] -> [Stabl]
parseCheckAndInterpret s dict = getResult $ interpret program dict
  where program = case parseStabl "" s
                  of Right pro -> pro
                     Left parseError -> error $ show parseError

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Map.Map String [Stabl] -> Result Stabl
interpret s dict = interpret' (Stack s, dict , [])

-- Data stack
newtype Stack a = Stack { getStack :: [a] }
-- Return stack
newtype Return a = Return { getReturn :: [a] }
-- Result stack
newtype Result a = Result { getResult :: [a] }

interpret' :: (Stack Stabl, Map.Map String [Stabl], [Stabl]) -> Result Stabl
interpret' (Stack [], dict, stack) = case (head stack) of 
  Lit num    -> Result stack
  Quotation quot' -> Result stack
  WordCall w -> error "type error!"
interpret' (Stack (Lit n : xs), dict, stack) = interpret' (Stack xs, dict, Lit n : stack)
interpret' (Stack (Quotation quot : xs), dict, stack) = interpret' (Stack xs, dict, Quotation quot : stack)
interpret' (Stack (WordCall s : xs), dict, stack) = 
    case s of 
            -- built-in words
            "add"   -> interpret' (Stack xs, dict, eval stack (+))
            "minus" -> interpret' (Stack xs, dict, eval stack (-))
            "mul"   -> interpret' (Stack xs, dict, eval stack (*))
            "div"   -> interpret' (Stack xs, dict, eval stack div)
            -- built-in stack combinators 
            "pop"   -> interpret' (Stack xs, dict, pop stack)
            "dup"   -> interpret' (Stack xs, dict, dup stack)
            "swap"  -> interpret' (Stack xs, dict, swap stack)
            "rot"   -> interpret' (Stack xs, dict, rot stack)
            "over"  -> interpret' (Stack xs, dict, over stack)
            
            -- applying a quotation 
            "apply"   -> case head' of Quotation quot -> interpret' (Stack xs, dict, apply quot dict tail') 
                                       other -> error $ "expected a quotation for word \"apply\", but was instead " ++ (show other)
              where head' = head stack
                    tail' = tail stack
            -- user-defined word 
            other   -> case lookup' of Just wordBody -> interpret' (Stack xs, dict, apply wordBody dict stack) 
                                       Nothing      -> error $ "undefined word: " ++ other ++ ". Program exiting." 
              where lookup' = Map.lookup other dict

eval :: [Stabl] -> (Int -> Int -> Int) -> [Stabl]
eval stack (¤) = let x = head stack
                     y = head $ pop stack 
                     res = (get y) ¤ (get x) 
                       where get t = case t of 
                               WordCall s -> error $ "was String, expected num: " ++ s 
                               Lit n -> n
                 in (Lit res):(pop $ pop stack)
         




 
                                                
  