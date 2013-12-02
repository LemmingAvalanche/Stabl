module Interpreter
       (
         parseCheckAndInterpret
       , interpret
       ) where

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

-- implementation of word-call

-- TODO: funker ikkje: det ser ut som om den blir evaluert i motsatt rekkefølge av det eg ønsker.
                -- eks:
                -- interpret ((Lit 1): [WordCall "dup"]) 
                -- evaluerer til:
                -- [Lit 1, Lit 1]
                -- Men eg vil at dette skal evaluere til det:
                -- interpret ((WordCall "dup"): [Lit 1] ) 
                -- altså at, viss eg møter ein quot som inneholder ein WordCall, så må eg evaluere den på toppen av stacken. 
                -- I dette tilfellet så burde dup bli pusha på stacken, og evalueres med å konsumere toppen av stacken, og legge igjen to nye verdiar (nemlig verdien som den konsumerte, pluss eit duplikat av den).
apply :: [Stabl] -> (Map.Map String [Stabl], [Stabl]) -> [Stabl]
apply quot (dict, stack) = apply' quot stack
  where apply' [] stack' = stack'
        apply' (x:xs) stack' = apply' xs $ getResult $ interpret (x:stack') dict

newApply :: [Stabl] -> Map.Map String [Stabl] ->  [Stabl] -> [Stabl]
newApply stack dict quot = getResult $ interpret (quot ++ stack) dict --- ...eller reverse quot?

-- TODO: implement checking
parseCheckAndInterpret :: String -> [Stabl]
parseCheckAndInterpret s = getResult $ interpret program undefined -- TODO: dict is undefined!!
  where program = case parseStabl "" s
                  of Right pro -> pro
                     Left parseError -> error (show parseError)
                     -- Left ParseError -> error "parse error!"

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Map.Map String [Stabl] -> Result Stabl
interpret s dict = interpret' (Stack s, dict , [])

-- TODO: implement these in the interpreter functions
-- Data stack
newtype Stack a = Stack { getStack :: [a] }
-- Return stack
newtype Return a = Return { getReturn :: [a] }
-- Result stack
newtype Result a = Result { getResult :: [a] }

-- TODO: fix all function-calls to also use a dictionary.
interpret' :: (Stack Stabl, Map.Map String [Stabl], [Stabl]) -> Result Stabl
interpret' (Stack [], dict, stack) = case (head stack) of 
  Lit num    -> Result stack
  Quotation quot' -> Result stack
  WordCall w -> error "type error!"
interpret' (Stack (Lit n : xs), dict, stack) = interpret' (Stack xs, dict, Lit n : stack)
interpret' (Stack (Quotation quot : xs), dict, stack) = interpret' (Stack xs, dict, Quotation quot : stack)
interpret' (Stack (WordCall s : xs), dict, stack) = 
    -- Built-in words  
    case s of -- TODO: hardcoded
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
            "apply"   -> case head' of Quotation quot -> interpret' (Stack xs, dict, newApply quot dict tail') -- funker ikkje 
                                       other -> error $ "expected a quotation for word \"apply\", but was instead " ++ (show other)
              where head' = head stack
                    tail' = tail stack
                                       
            -- user-defined word 
            other   -> case lookup' of Just wordBody -> interpret' (Stack xs, dict, newApply wordBody dict stack) -- TODO: implement apply
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
         




 
                                                
  