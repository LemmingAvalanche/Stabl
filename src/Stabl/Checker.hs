-- | Type checking and general semantic analysis.
import Data.List
import Data.Function (on)
import Data.Maybe (catMaybes)
import Control.Category ((>>>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Parser

{- -- Helper functions for Checker -- -}

builtInWords :: Set.Set String
builtInWords = Set.fromList ["pop", "dup", "swap", "over", "rot"]

-- | Checks if all word definitions have a unique name
allUnique :: [WordDef] -> Bool
allUnique = null . allDuplicates

-- | All duplicates of a list. Duplicates are WordDefs that have the same name. 
-- Uses an arrow so that the dataflow can be read from left to right... f . g = g >>> f
allDuplicates :: [WordDef] -> [WordDef]
allDuplicates =     sortBy (compare `on` fst)
                    >>> groupBy ((==) `on` fst)
                    >>> filter lengthGreaterThan1
                    >>> concat
  where lengthGreaterThan1 = length >>> (>1)
        
checkTopDefinitions :: [WordDef] -> [String]        
checkTopDefinitions program = validWordCalls program set 
  where set = builtInWords `Set.union` (Set.fromList $ map fst program)

-- TODO: store top-level definitions and type-check
-- | Tests that all top-level definitions only refer to other top-level definitions (words) and built-in words. Returns a list of errors. 
validWordCalls :: [WordDef] -> Set.Set String -> [String]
validWordCalls program set = catMaybes $ map (`validWordCall` set) program

validWordCall :: WordDef -> Set.Set String -> Maybe String
validWordCall (name,quot) set = if name `Set.member` set
                         then Nothing 
                         else Just $ 
                              "Error: word " ++ name ++ " is not a defined word. Was called in the definition: " ++ "\n\t" ++ (show quot)
                              
                              