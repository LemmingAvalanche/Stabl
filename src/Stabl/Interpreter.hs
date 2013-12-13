module Interpreter
       (
         parseCheckAndInterpret
       , interpret
       , apply
       ) where
import Control.Applicative
import Control.Monad.Error
import qualified Data.Map as Map

import Parser
import Text.ParserCombinators.Parsec.Error

data StablErr = StackUnderflow String
                | Parser ParseError
                | TypeMismatchErr { 
                    expected :: String
                  , actual :: String }
                | UndefinedWord String
                | Default String

instance Error StablErr where
  noMsg = Default "An error has occured"
  strMsg = Default

showErr (StackUnderflow str) = "Stack underflow: the combinator " ++ str ++ " was called on the stack, but there were not enough elements on the stack."
showErr (Parser err) = show err
showErr (TypeMismatchErr exp act) = "Typemismatch error: expected " ++ exp ++ ", but got " ++ act
showErr (UndefinedWord str) = "Undefined word error: tried to call word " ++ str ++ ", but it is not defined."

type CanErr = Either StablErr

type Dict = Map.Map String [Stabl]

top :: [Stabl] -> CanErr Stabl
top [] = Left $ StackUnderflow "top"
top (x:_) = Right x

pop :: [Stabl] -> CanErr [Stabl]
pop [] = Left $ StackUnderflow "pop"
pop (x:xs) = Right xs

-- | See - Forth
dup :: [Stabl] -> CanErr [Stabl]
dup [] = Left $ StackUnderflow "dup"
dup (x:xs) = Right $ x:x:xs

-- | See - Forth
swap :: [Stabl] -> CanErr [Stabl]
swap [] = Left $ StackUnderflow "swap"
swap [_] = Left $ StackUnderflow "swap"
swap (x:y:xs) = Right $ y:x:xs

-- | See - Forth 
rot :: [Stabl] -> CanErr [Stabl]
rot [] = Left $ StackUnderflow "rot"
rot [_] = Left $ StackUnderflow "rot"
rot [_,_] = Left $ StackUnderflow "rot"
rot (x:y:z:xs) = Right $ z:x:y:xs

-- | See - Forth
over :: [Stabl] -> CanErr [Stabl]
over [] = Left $ StackUnderflow "over"
over [_] = Left $ StackUnderflow "over"
over (x:y:xs) = Right $ y:x:y:xs

apply :: [Stabl] -> Dict -> [Stabl] -> CanErr [Stabl]
apply stack dict quot = interpret ((reverse quot) ++ stack) dict
 
-- TODO: fix to make total
parseCheckAndInterpret :: String -> Dict -> CanErr [Stabl]
parseCheckAndInterpret s dict = either
                                (Left . Parser)
                                (\prog -> interpret prog dict) 
                                (parseStabl "" s)


-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Dict -> CanErr [Stabl]
interpret s dict = interpret' (s, dict , [])

-- Data stack
-- newtype Stack a = Stack { getStack :: [a] }
-- Return stack
-- newtype Return a = Return { getReturn :: [a] }
-- Result stack
-- newtype Result a = Result { getResult :: [a] }

interpret' :: ([Stabl], Dict, [Stabl]) -> CanErr [Stabl]
interpret' ([], dict, stack) = case (head stack) of 
  Lit num    -> Right stack
  Quotation quot' -> Right stack
  WordCall w -> Left $ TypeMismatchErr {
    expected = "a stack with only values"
    , actual = "an unevaluated word"}
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
                                       other -> Left $ TypeMismatchErr {
                                         expected = "a quotation"
                                         , actual = "the word: " ++ (show other)}
              where head' = head stack
                    tail' = tail stack
            -- user-defined word 
            other   -> case lookup' of Just wordBody -> eitherR 
                                                        (\res -> interpret' (xs, dict, res)) 
                                                        (apply wordBody dict stack) 
                                       Nothing       -> Left $ UndefinedWord other
              where lookup' = Map.lookup other dict

-- TODO: move to where-clase in interpret'?
ifSuccessArithmetic stack' dict' op = eitherR 
                                      (\res -> interpret' (stack', dict', res)) 
                                      (eval stack' op)

ifSuccessComb stack dict retStack comb = eitherR
                                         (\res -> interpret' (stack, dict, res)) 
                                         (comb retStack)

eval :: [Stabl] -> (Integer -> Integer -> Integer) -> CanErr [Stabl]
eval stack op = let x = top stack
                    y = eitherR top (pop stack) 
                    res = liftA2 op (get y) (get x) :: CanErr Integer
                       where get = eitherR 
                                   (\res -> case res of 
                                                   WordCall s -> Left $ TypeMismatchErr { expected = "an int", actual = "the word: " ++ s }
                                                   Lit n -> Right n)
                 in eitherR (\result -> Right $ (Lit result):stack'') res where stack'' = tail $ tail stack -- Uses partial functions, BUT, this is safe since the evaluation can't have come this far if there indeed weren't at least two items on top of the stack, since then we would not have been able to use arithmetic on the top two elements of the stack. 

-- | Evaluates the right value in the first given function if Right value and returns an either value; propagates the error if Left value
eitherR :: (b -> Either a c) -> Either a b -> Either a c
eitherR = either Left         




 
                                                
  