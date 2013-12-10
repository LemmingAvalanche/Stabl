-- REPL inspiried by the basic REPL described in Write Yourself a Lisp in 48 Hours

import System.IO
import System.Environment

import qualified Data.Map as Map

import Parser
import Interpreter

flushStr :: String -> IO ()
flushStr str =    putStr str 
               >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt =    flushStr prompt 
                    >> getLine              
                    
evalPrintReturn :: String -> ([Stabl], Map.Map String [Stabl]) -> IO ([Stabl] ,Map.Map String [Stabl])
evalPrintReturn str@('d':'e':'f':rest) (stabl,dict) = let (name, body) = parseWordDefUnsafely str
                                                          in do putStrLn $ "new word defined: " ++ name
                                                                return (stabl, Map.insert name body dict) 
evalPrintReturn str (stabl, dict) = do let expr = parseStablUnsafely str
                                       let result = apply expr dict stabl
                                       putStrLn $ show result 
                                       return (result, dict)                                       
                                       
emptyState = ([], Map.empty)

until_ :: (String -> Bool) -> IO String -> ([Stabl], Map.Map String [Stabl]) -> IO ([Stabl], Map.Map String [Stabl])
until_ pred prompt state = do
  result <- prompt
  if pred result
     then return emptyState
     else do state' <- evalPrintReturn result state
             until_ pred prompt state'
          
-- Loop infinitely until "quit" command
runRepl :: IO ([Stabl], Map.Map String [Stabl])
runRepl = until_ (== "quit") (readPrompt "Stabl>>> ") emptyState

main :: IO ([Stabl], Map.Map String [Stabl])
main = do args <- getArgs 
          -- number of arguments
          case length args of 0 -> runRepl
                              otherwise -> do putStrLn "program expects no arguments"
                                              return emptyState -- 



