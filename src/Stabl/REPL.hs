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
evalString :: String -> [Stabl]
evalString = parseCheckAndInterpret
              
evalPrintReturn :: String -> [Stabl] -> IO [Stabl]
evalPrintReturn str stabl = do let expr = parseStablUnsafely str
                               let result = newApply expr Map.empty stabl
                               putStrLn $ show result 
                               return result

until_ :: (String -> Bool) -> IO String -> [Stabl] -> IO [Stabl]
until_ pred prompt stabl = do
  result <- prompt
  if pred result
     then return [] -- obs: var return ()
     else do stabl' <- evalPrintReturn result stabl
             until_ pred prompt stabl'
          
-- Loop infinitely until "quit" command
runRepl :: IO [Stabl]
runRepl = until_ (== "quit") (readPrompt "Stabl>>> ") []

main :: IO [Stabl]
main = do args <- getArgs -- trenger ingen argument
          -- number of arguments
          case length args of 0 -> runRepl
                              otherwise -> return [] -- for å få det til å kompilere



