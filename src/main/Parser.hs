module Parser 
       (
         Stabl(..)
       , parseStabl
       ) where


import Text.ParserCombinators.Parsec hiding (many, (<|>))-- AST for the language. This should really only be the list of legal tokens of the language. 
import Control.Applicative 
import Control.Monad (liftM)

-- defines a word (function). Syntax:
-- def <name> (quotation or stablToken)
def = "def"
openQuot = "["
closeQuot = "]";

-- TODO: legg til alternativ: def <name> StablToken. Isåfall må eg oppdatere parseren.
data WordDef = Def Word Quot 
               deriving (Show,Read,Eq)

type Word = String
-- quotation. see: cat-lang
type Quot = [Stabl]

data Stabl = WordCall String    -- Word
               | Lit Int    -- Literal
                 deriving (Show,Read,Eq) -- TODO: meir?

-- | A word definition
wordDef :: Parser WordDef
wordDef = do
  string def :: Parser String
  name <- word'
  quot' <- quotation
  return $ Def name quot'
  
-- Trying to implement wordDef, but with applicative functors
{-
wordDef' :: Parser WordDef
wordDef' =     string def
           *>  word'
           <*> quotation
-}

quotation :: Parser Quot
quotation = between (string openQuot) (string closeQuot) sequenceStablToken

-- | The usual integer
int :: Parser Stabl
int =     fmap (Lit . read) $ many1 digit
       <* notFollowedBy letter -- TODO: change? a string like "225+-" shouldn't be parsed by consuming "225" and discarding "+-". 


-- | called "word prime" in order to be sure that it doesn't conflict
-- | with any function named "word" in Parsec.
word' :: Parser Word
word' = many1 alphaNum

wordCall :: Parser Stabl
wordCall = fmap WordCall word' -- TODO: change to any character that is not whitespace? It should be possible to use strings like "+" and "-" as identifiers of words. 

-- | A token that can't be parsed as an int literal is assumed to be a word. If a token can't be parsed as an int, the lookahead fails without 1. consuming any input 2. propagating an error.  
stablToken :: Parser Stabl
stablToken =  (try int) <|> wordCall

sequenceStablToken = stablToken `sepBy` spaces 

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl =    discardWhitespace
             *> parse sequenceStablToken
             <* discardWhitespace
                 where discardWhitespace = parse $ many space :: SourceName -> String -> Either ParseError String
    -- TODO: found a bug: this doesn't seem to help with whitespace at start of input (returns empty list of tokens) nor at end of input (throws an 

