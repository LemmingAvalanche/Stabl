

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

-- TODO: check word-definitions, such as if it only uses words that have been defined at this point?
evalPrintReturn :: String -> ([Stabl], Dict) -> IO ([Stabl], Dict)
evalPrintReturn str (stabl, dict) = case parseStabl "" str of Left err -> putStrLn (show err)
                                                                       >> return (stabl, dict)
                                                              Right expr -> let result = apply expr dict stabl
                                                                            in case result of Left err' -> putStrLn (show err') 
                                                                                                           >> return (stabl, dict)
                                                                                              Right (stabl', dict') -> putStrLn (show stabl') 
                                                                                                                       >> return (stabl', dict)
                                       
emptyState = ([], Map.empty)

until_ :: (String -> Bool) -> IO String -> ([Stabl], Dict) -> IO ([Stabl], Dict)
until_ pred prompt state = do
  result <- prompt
  if pred result
     then return emptyState
     else do state' <- evalPrintReturn result state
             until_ pred prompt state'
          
-- Loop infinitely until "quit" command
runRepl :: IO ([Stabl], Dict)
runRepl = until_ (== "quit") (readPrompt "Stabl>>> ") emptyState

main :: IO ([Stabl], Dict)
main = do args <- getArgs 
          -- number of arguments
          case length args of 0 -> runRepl
                              otherwise -> do putStrLn "program expects no arguments"
                                              return emptyState -- 



