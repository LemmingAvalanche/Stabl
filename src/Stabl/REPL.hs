-- TODO: implement :)

-- REPL library(?)
-- http://hackage.haskell.org/package/repl-0.95/docs/Language-Haskell-Repl.html

-- Inspirert av Write Yourself a Lisp ...

import System.IO
--eg måtte visst importere denne for å få getArgs i main
import System.Environment


import Parser
import Interpreter

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString = return . show . parseCheckAndInterpret

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action
          
-- Loop infinitely until "quit" command
runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Stabl>>> ") evalAndPrint

main :: IO ()
main = do args <- getArgs
          -- number of arguments
          case length args of 0 -> runRepl
                              1 -> evalAndPrint $ head args
                              otherwise -> putStrLn "Program takes only 0 or 1 argument"



