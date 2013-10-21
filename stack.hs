import Data.Char (isDigit)
import Data.List (words, any, all)

underflow = "stack underflow!"

pop [] = error underflow
pop (x:xs) = xs

dup [] = error underflow
dup (x:xs) = (x:x:xs)

type Token = String

tokenize :: String -> [Token]
tokenize = words
 
interpret :: String -> Int
interpret str = let
                tokens = tokenize str 
                in interpret' (tokens,[])
                   where 
                     interpret' :: ([Token],[Int]) -> Int
                     interpret' ([],stack) = head stack
                     interpret' ((token:xs),stack)
                       | all isDigit token = let num = read token in interpret' (xs,(num:stack))
                       | token == "+" || token == "-" || token == "*" || token == "/" = 
                         let (¤) = case token of
                               "+" -> (+)
                               "-" -> (-)
                               "*" -> (*)
                               "/" -> div
                               in eval (¤) stack where
                                                       eval (¤) stack' = 
                                                         let x = head stack'
                                                             y = head $ pop stack'
                                                             res = x ¤ y
                                                         in interpret' (xs,(res:(pop $ pop stack')))
         




 
                                                
  