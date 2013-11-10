-- | Type checking and general static semantic analysis.
import Data.List
import Data.Function (on)
import Control.Category ((>>>))
import qualified Data.Map.Strict as Map

import Parser

-- | Checks if all word definitions have a unique name
-- NOTE: tested once for positive and negative input, and it worked.
allUnique :: [WordDef] -> Bool
allUnique = null . allDuplicates

-- | All duplicates of a list. Duplicates are WordDefs that have the same name. 
-- Uses an arrow so that the dataflow can be read from left to right... I thought it might be more readable for longer function compositions.
allDuplicates :: [WordDef] -> [WordDef]
allDuplicates =     sortBy (compare `on` fst)
                    >>> groupBy ((==) `on` fst)
                    >>> filter ((>1) . length)
                    >>> concat
                    --head . filter ((>1) . length)
                -- . group . sortBy (compare `on` fst)