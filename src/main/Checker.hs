-- | Type checking and general static semantic analysis.
import Data.List
import qualified Data.Map.Strict as Map

import Parser

-- | Checks if all word definitions have a unique name
-- NOTE: tested once for positive and negative input, and it worked.
allUnique :: [WordDef] -> Bool
allUnique list = (length unique) == (length list) 
  where unique = nubBy (\f s -> (wordName f) == (wordName s)) list

-- | Make a list of WordDef into a list of tuples, so that it can easily be translated to a Map.
zipDef :: [WordDef] -> [(Word, Quot)]
zipDef list = zip 
               [wordName def | def <- list] 
               [wordQuot def | def <- list]

toDict :: [WordDef] -> Map.Map Word Quot
toDict = Map.fromList . zipDef
