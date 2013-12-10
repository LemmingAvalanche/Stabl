-- TODO: implement :)b

-- REPL library(?)
-- http://hackage.haskell.org/package/repl-0.95/docs/Language-Haskell-Repl.html

-- Inspirert av Write Yourself a Lisp ...
import Control.Applicative
import Control.Exception
import Data.IORef
import System.IO
--eg måtte visst importere denne for å få getArgs i main
import System.Environment

import qualified Data.Map as Map

import Parser
import Interpreter

-- The stack of the program, and its dictionary (map from strings to words)
type State = IORef ([Stabl], Map.Map String [Stabl])

flushStr :: String -> IO ()
flushStr str =    putStr str 
               >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt =    flushStr prompt 
                    >> getLine

-- vits med?
-- evalString :: String -> [Stabl]
-- evalString = parseCheckAndInterpret Map.empty
              
evalPrintReturn :: String -> ([Stabl], Map.Map String [Stabl]) -> IO ([Stabl] ,Map.Map String [Stabl])
evalPrintReturn str@('d':'e':'f':rest) (stabl,dict) = let (name, body) = parseWordDefUnsafely str
                                                          in do putStrLn "new word defined."
                                                                return (stabl, Map.insert name body dict)
                                                            
evalPrintReturn str (stabl, dict) = do let expr = parseStablUnsafely str
                                       let result = newApply expr dict stabl
                                       putStrLn $ show result 
                                       return (result, dict)
                                       
emptyState = ([], Map.empty)

until_ :: (String -> Bool) -> IO String -> ([Stabl], Map.Map String [Stabl]) -> IO ([Stabl], Map.Map String [Stabl])
until_ pred prompt state = do
  result <- prompt
  if pred result
     then return emptyState -- obs: var return ()
     else do -- putStrLn result -- debug
             state' <- evalPrintReturn result state
             until_ pred prompt state'
          
-- Loop infinitely until "quit" command
runRepl :: IO ([Stabl], Map.Map String [Stabl])
runRepl = until_ (== "quit") (readPrompt "Stabl>>> ") emptyState

main :: IO ([Stabl], Map.Map String [Stabl])
main = do args <- getArgs -- trenger ingen argument
          -- number of arguments
          case length args of 0 -> runRepl
                              otherwise -> return emptyState -- for å få det til å kompilere



