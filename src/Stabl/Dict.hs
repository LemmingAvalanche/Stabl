{-

Module that contains a dictionary that might be used instead of Data.Map in the interpreter. With this dictionary, one can implement words like pop-def that pops the definitions from the dictionary. This can for example be used in a word like this:

[[<something>] 'word1 def 
 [<something else>] 'word2 def
 
 <use word1 and word2 in the "body" of this word>
 pop-def
 pop-def
] 'word3 def

Here we defined two new words at the beginning of word3. In order to avoid these definitions to "spill over" to other variables (like dynamic scoping), we use pop-def two times in order to remove them from the essentially gobal dictionary that is being used. In this way, with a little care and discipline, we get a kind of "lexical scoping" in an essentially dynamically scoped context. 

-}
module Dict 
       (
         Dict
       , pushDictDef
       , popDictDef 
       , lookupDef
       ) where

import qualified Data.Map as M
import Data.Maybe
import Data.List.NonEmpty

import Parser

type InternalDict = M.Map String (NonEmpty [Stabl])

-- NOTE: use newtype instead?
type Dict = ([String], InternalDict)

pushDictDef :: String -> [Stabl] -> Dict -> Dict
pushDictDef str prog (keyStack, internal) = (str:keyStack
                                             , M.insert str (NonEmpty prog rest) internal)
                               where rest = maybe 
                                            [] 
                                            nonEmptyToList 
                                            (M.lookup str internal)
           
-- | Pops the top key from the internal stack and removes the key and corresponding value from the internal dict. If the key stack is empty, Nothing is returned. 
popDictDef :: Dict -> Maybe Dict
popDictDef ([], _) = Nothing -- Empty keyStack: can not pop
popDictDef (k:keyStack, internal) = Just (keyStack, pop k internal)
                                          -- invariant: as long as a String str exists on the stack of keys, there should also be a corresponding key and value in the internal Map. So using fromJust here is fine. 
                           where pop k inter = case neTail $ fromJust $ M.lookup k inter of
                                                    [] -> M.delete k inter
                                                    (first:rest) -> M.insert k (NonEmpty first rest) inter

-- | The value that is returned is at the top of the stack which is associated with the key
lookupDef :: String -> Dict -> Maybe [Stabl]
lookupDef str (_, internal) = fmap neHead (M.lookup str internal)
