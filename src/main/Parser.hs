module Parser 
       (
         Stabl(..)
       , parseStabl
       ) where


import Text.ParserCombinators.Parsec hiding (many, (<|>))-- AST for the language. This should really only be the list of legal tokens of the language. 
import Control.Applicative

-- defines a word (function). Syntax:
-- def <name> (quotation or stablToken)
def = "def"
openQuot = "["
closeQuot = "]";

-- TODO: legg til alternativ: def <name> StablToken. Isåfall må eg oppdatere parseren.
data WordDef = Def Word Quot
               deriving (Show,Read,Eq,Ord)

-- TODO: give better name
data Choice = Stabl | Quot deriving (Show,Read,Eq,Ord)

type Word = String
-- quotation. see: cat-lang
type Quot = [Stabl]

data Stabl = WordCall Word    -- Word
               | Lit Int    -- Literal
                 deriving (Show,Read,Eq,Ord) -- TODO: meir?

-- | A word definition  
wordDef :: Parser WordDef 
wordDef =  (string def *> whitespace1)
           *> liftA2 Def 
           (word' <* whitespace1) 
           quotation -- NOTE: eg skulle gjerne ha lagt til <|> stabl, altså at ein Def kan bestå av eit namn og ein quotation ELLER eit namn og ein stablToken

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

-- TODO: give better name
sequenceStablToken =    whitespace         
                     *> stablToken `sepBy` whitespace1
                     <* whitespace -- BUG: gives exception if there is whitespace at end of string 

whitespace = many space

whitespace1 :: Parser String
whitespace1 = many1 space

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = parse sequenceStablToken
    -- TODO: found a bug: this doesn't seem to help with whitespace at start of input (returns empty list of tokens) nor at end of input (throws an 

