
module Interpreter
       (
         parseAndInterpret
       , parseCheckAndInterpret
       , interpret
       , apply
       , CanErr
       , Dict
       ) where
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import qualified Data.Map as Map

import Parser
import Text.ParserCombinators.Parsec.Error

data StablErr = StackUnderflow String
                | ParserErr ParseError
                | TypeMismatchErr { 
                    expected :: String
                  , actual :: String }
                | UndefinedWord String
                | Default String 
                  --- TODO? : deriving (Read,Show,Ord,Eq) -- ... more?

instance Error StablErr where
  noMsg = Default "An error has occured"
  strMsg = Default

instance Show StablErr where
  show (StackUnderflow str) = "Stack underflow: the combinator " ++ 
                              str ++ " was called on the stack, but there were not enough elements on the stack."
  show (ParserErr err) = show err
  show (TypeMismatchErr exp act) = "Typemismatch error: expected " ++ exp ++ ", but got " ++ act
  show (UndefinedWord str) = "Undefined word error: tried to call word " ++ str ++ ", but it is not defined."
  show (Default str) = str

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

apply :: [Stabl] -> Dict -> [Stabl] -> CanErr ([Stabl], Dict)
apply stack dict quot = interpret (reverse quot ++ stack) dict

{-
Parse a raw string and return the output stack (or an error).x
-}
-- TODO: refaktorer denne og parseCheckAndInterpret
parseAndInterpret :: String -> CanErr [Stabl]
parseAndInterpret s = either 
                      (Left . ParserErr) 
                      (\prog -> fmap fst $ interpret prog Map.empty) 
                      (parseStabl "" s)
 
-- TODO: fix to make total
parseCheckAndInterpret :: String -> Dict -> CanErr ([Stabl], Dict)
parseCheckAndInterpret s dict = either
                                (Left . ParserErr)
                                (\prog -> interpret prog dict) 
                                (parseStabl "" s)

-- TODO: fix argument to the readFile word: should probably be a string, but strings are not part of the language as of yet. So for now it is wrapped in a quotation
-- effectful (IO) interpreting: extends the interpret function with words to read programs from files.

                                   -- Monad stack?
-- TODO: non-exhaustive number of cases. I should add the rest (which should just be a "wrapper" for calling pure (non-IO) words).
interpret_io :: [Stabl] -> Dict -> IO (CanErr ([Stabl], Dict))
interpret_io s dict = interpret_io' (s, dict, [])

-- built-in io word readFile
interpret_io' :: ([Stabl], Dict, [Stabl]) -> IO (CanErr ([Stabl], Dict))
interpret_io' (((Quotation [WordCall path]):(WordCall "readFile"):rest), dict, stack) = do
  prog <- parseFile path
  -- OBS: uses interpret, so the file that the program originates in can not use programs from other files!
  let result = case prog of Left err -> Left $ ParserErr err -- TODO: refactor... 
                            Right result -> interpret result dict  -- Since this uses a pure function, this means that effectful - such as reading a file - can't be called *inside a file*. This might not be useful for my purposes, anyway, but it's something that should be kept in mind. 
  case result of error@(Left _) -> return error
                 Right (stack', dict') -> interpret_io' (rest, dict', stack')
-- TODO: program the rest of the cases
interpret_io' (stack, dict, retStack) = error "Not implemented yet!!"
  
-- TODO: make an interpret function that takes (Either ParseError [Stabl]) as input instead of [Stabl] ?

-- | interpret a program given by a quotation.
interpret :: [Stabl] -> Dict -> CanErr ([Stabl], Dict)
interpret s dict = interpret' (s, dict , [])

-- idé: refaktor til å bruke monad transformer? - EitherT State, noko sånt som det
interpret' :: ([Stabl], Dict, [Stabl]) -> CanErr ([Stabl], Dict)
interpret' ([], dict, []) = Right ([], dict) 
interpret' ([], dict, stack) = case (head stack) of
  LitInt num    -> Right (stack, dict)
  Quotation quot' -> Right (stack, dict)
  WordCall w -> Left $ TypeMismatchErr {
    expected = "a stack with only values"
    , actual = "an unevaluated word" }
interpret' (LitInt n : xs, dict, stack) = interpret' (xs, dict, LitInt n : stack)
interpret' (Quotation quot : xs, dict, stack) = interpret' (xs, dict, Quotation quot : stack)
interpret' (WordCall s : xs, dict, stack) =
            -- word definition
  case s of "def" ->
              case stack of Quotation [WordCall call] : Quotation body : rest -> interpret' (xs, Map.insert call body dict, rest)
                            rest -> Left TypeMismatchErr {
                              expected = "a quotation with a single word (name of word) and a quotation (the 'body' of the word)",
                              actual = "the rest of the stack: " ++ show stack} 
            -- built-in words
            -- OBS: fortsatt bug her? eller fiksa eg alt som var gale her?
            "+"   -> ifSuccessArithmetic xs stack dict (+)
            "-" -> ifSuccessArithmetic xs stack dict (-)
            "*"   -> ifSuccessArithmetic xs stack dict (*)
            "/"   -> ifSuccessArithmetic xs stack dict div 
            
            -- built-in comparators
            "==" -> ifSuccessArithmetic xs stack dict (eq) -- OBS: ved å bruke ifSuccess... så funker denne kun bå Integer, men den bør også funke på andre typer (some Char).
            ">" -> ifSuccessArithmetic xs stack dict (gt)
            "<" -> ifSuccessArithmetic xs stack dict (lt)

            -- built-in stack combinators 
            "pop"   -> ifSuccessComb xs dict stack pop 
            "dup"   -> ifSuccessComb xs dict stack dup 
            "swap"  -> ifSuccessComb xs dict stack swap 
            "rot"   -> ifSuccessComb xs dict stack rot
            "over"  -> ifSuccessComb xs dict stack over 

            -- control flow
            -- syntax: <flag> <if-branch> <else-branch> if (branches are represented as quotations)
                              -- OBS: rett pattern-matching?
            "if"    -> case stack of (Quotation elseBranch) : (Quotation ifBranch) : flag : rest -> if flag == (LitInt 1) -- true! 
                                                                                                    then applyWithBranch ifBranch
                                                                                                    else applyWithBranch elseBranch
                                                                                                      where applyWithBranch branch = apply branch dict rest 
                                                                                                                                     >>= \(res', dict') -> interpret' (xs, dict', res')       
                                     _ -> Left $ TypeMismatchErr {expected = "well-formed if expression", actual = "not well-formed if expression"} -- TODO: give more useful error-message. 
            
            -- applying a quotation 
            -- OBS: all kode under dette bør kanskje refaktorerast
            "apply"   -> case head' of Quotation quot -> apply quot dict tail' >>= \(res', dict') -> interpret' (xs, dict', res')
                                       other -> Left $ TypeMismatchErr {
                                         expected = "a quotation"
                                         , actual = "the word: " ++ show other}
              where head' = head stack
                    tail' = tail stack
            -- user-defined word                        
            other   -> case lookup' of Just wordBody -> apply wordBody dict stack >>= \(ret', dict') -> interpret' (xs, dict', ret')
                                       Nothing       -> Left $ UndefinedWord other
              where lookup' = Map.lookup other dict

-- TODO: move to where-clase in interpret'?

-- TODO: rett rekkefølge på operatorane?
eq :: Integer -> Integer -> Integer
eq y x = if y == x then 1 else 0
-- greater than 
gt :: Integer -> Integer -> Integer
gt y x = if y > x then 1 else 0
-- less than
lt y x = if y < x then 1 else 0


-- Check if the two stack values have the same type: if they are both a wrapped int, a wrapped quotation, etc.
sameType :: Stabl -> Stabl -> Bool
sameType (LitInt _) (LitInt _) = True
sameType (LitChar _) (LitChar _) = True
sameType (Quotation _) (Quotation _) = True
sameType (WordCall _) (WordCall _) = True
sameType _ _ = False

ifSuccessArithmetic :: [Stabl] -> [Stabl] -> Dict -> (Integer -> Integer -> Integer) -> CanErr ([Stabl], Dict)
ifSuccessArithmetic stack retStack dict op = eval retStack op
                                             >>= 
                                             resApplyInterpret' (stack, dict)

ifSuccessComb :: [Stabl] -> Dict -> [Stabl] -> ([Stabl] -> CanErr [Stabl]) -> CanErr ([Stabl], Dict)
ifSuccessComb stack dict retStack comb = comb retStack
                                         >>= 
                                         resApplyInterpret' (stack,dict)

resApplyInterpret' :: ([Stabl], Dict) -> [Stabl] -> CanErr ([Stabl], Dict)
resApplyInterpret' (stack, dict) res = interpret' (stack, dict, res)                                         

eval :: [Stabl] -> (Integer -> Integer -> Integer) -> CanErr [Stabl]
eval stack op = let x = top stack
                    y = pop stack >>= top
                    res = liftA2 op (get y) (get x)
                       where get = (>>=
                                   (\res -> case res of 
                                                   WordCall s -> Left $ TypeMismatchErr { expected = "an int", actual = "the word: " ++ s }
                                                   LitInt n -> Right n)
                                   )
                 in res >>= (\result -> return $ (LitInt result):stack'') 
  where stack'' = (tail . tail) stack -- Uses partial functions, BUT, this is safe since the evaluation can't have come this far if there indeed weren't at least two items on top of the stack, since then we would not have been able to use arithmetic on the top two elements of the stack. 




 
