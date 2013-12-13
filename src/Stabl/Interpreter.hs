module Interpreter
       (
         parseCheckAndInterpret
       , interpret
       , apply
       ) where
import Control.Applicative
import qualified Data.Map as Map

import Parser

top [] = Nothing
top (x:_) = x

pop [] = Nothing
pop (x:xs) = Just xs

-- | See - Forth
dup [] = Nothing
dup (x:xs) = Just $ x:x:xs

-- | See - Forth
swap [] = Nothing
swap [_] = Nothing
swap (x:y:xs) = Just $ y:x:xs

-- | See - Forth
rot [] = Nothing
rot [_] = Nothing
rot [_,_] = Nothing
rot (x:y:z:xs) = Just $ z:x:y:xs

-- | See - Forth
over [] = Nothing
over [_] = Nothing
over (x:y:xs) = Just $ y:x:y:xs

apply :: [Stabl] -> Map.Map String [Stabl] -> [Stabl] -> Either String [Stabl]
apply stack dict quot = interpret ((reverse quot) ++ stack) dict

-- TODO: fix to make total
parseCheckAndInterpret :: String -> Map.Map String [Stabl] -> Either String [Stabl]
parseCheckAndInterpret s dict = either 
                                (\err -> Left $ "Parse error: " ++ (show err)) 
                                (\prog -> interpret prog dict) 
                                (parseStabl "" s)

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Map.Map String [Stabl] -> Either String [Stabl]
interpret s dict = interpret' (s, dict , [])

-- Data stack
-- newtype Stack a = Stack { getStack :: [a] }
-- Return stack
-- newtype Return a = Return { getReturn :: [a] }
-- Result stack
-- newtype Result a = Result { getResult :: [a] }

interpret' :: ([Stabl], Map.Map String [Stabl], [Stabl]) -> Either String [Stabl]
interpret' ([], dict, stack) = case (head stack) of 
  Lit num    -> Right stack
  Quotation quot' -> Right stack
  WordCall w -> Left $ "type error: the returning stack contained an unevaluated word: " ++ w
interpret' (Lit n : xs, dict, stack) = interpret' (xs, dict, Lit n : stack)
interpret' (Quotation quot : xs, dict, stack) = interpret' (xs, dict, Quotation quot : stack)
interpret' (WordCall s : xs, dict, stack) = 
   case s of 
            -- built-in words
     -- TODO: refactor duplication
            "add"   -> ifSuccessArithmetic xs dict (+)
            "minus" -> ifSuccessArithmetic xs dict (-)
            "mul"   -> ifSuccessArithmetic xs dict (*)
            "div"   -> ifSuccessArithmetic xs dict div 
              
            -- built-in stack combinators 
            "pop"   -> ifSuccessComb xs dict stack pop 
            "dup"   -> ifSuccessComb xs dict stack dup 
            "swap"  -> ifSuccessComb xs dict stack swap 
            "rot"   -> ifSuccessComb xs dict stack rot
            "over"  -> ifSuccessComb xs dict stack over 
            
            -- applying a quotation 
            "apply"   -> case head' of Quotation quot -> eitherR (\res -> interpret' (xs, dict, res)) (apply quot dict tail')
                                       other -> Left $ "expected a quotation for word \"apply\", but was instead " ++ (show other)
              where head' = head stack
                    tail' = tail stack
            -- user-defined word 
            other   -> case lookup' of Just wordBody -> eitherR (\res -> interpret' (xs, dict, res)) (apply wordBody dict stack) 
                                       Nothing       -> Left $ "undefined word: " ++ other 
              where lookup' = Map.lookup other dict

-- TODO: move to where-clase in interpret'?
ifSuccessArithmetic stack' dict' op = eitherR 
                                      (\res -> interpret' (stack', dict', res)) 
                                      (eval stack' op)

ifSuccessComb stack dict retStack comb = maybe 
                                         (Left "Stack Underflow Error.")
                                         (\res -> interpret' (stack, dict, res)) 
                                         (comb retStack)

eval :: [Stabl] -> (Int -> Int -> Int) -> Either String [Stabl]
eval stack (¤) = let x = top stack
                     y = top $ pop stack 
                     res = liftA2 (¤) (get y) (get x) :: Either String Int
                       where get t = case t of 
                               WordCall s -> Left $ "was a word, expected num: " ++ s 
                               Lit n -> Right n
                 in eitherR (\result -> Right $ (Lit result):(pop $ pop stack)) res

-- | Evaluates the right value in the first given function if Right value and returns an either value; propagates the error if Left value
eitherR :: (b -> Either a c) -> Either a b -> Either a c
eitherR = either Left         




 
                                                
  