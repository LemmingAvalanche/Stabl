-- | Type checking and general static semantic analysis.
import Data.List

import Parser

-- | Checks if all word definitions have a unique name
-- NOTE: tested once for positive and negative input, and it worked.
allUnique :: [WordDef] -> Bool
allUnique list = (length unique) == (length list) 
  where unique = nubBy (\f s -> (wordName f) == (wordName s)) list