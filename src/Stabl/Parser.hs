module Parser 
       (
         WordDef
       , Word
       , Stabl(..)
       , Quot
       , parseDecl
       , parseStabl
       , parseStablUnsafely
       ) where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as Token
import Control.Applicative

-- defines a word (function)
-- Syntax:
--   def <name> { Stabl* }
def = "def"
openDef = "{"
closeDef = "}"

openQuot = "["
closeQuot = "]"

type WordDef = (Word, Quot)

-- TODO: give better name
data Choice = Stabl | Quot deriving (Show,Read,Eq,Ord)

type Word = String
-- quotation. see: cat-lang
-- NOTE: can use Quot as a collection/list: don't need a separate collection/list syntax and semantics
--- (though should probably have some primitive functions, like cons and append).
type Quot = [Stabl] 

data Stabl = WordCall Word
           | Quotation Quot
           | Lit Int    -- Literal
                 deriving (Show,Read,Eq,Ord) -- more?

-- | A word definition  
wordDef :: Parser WordDef
wordDef =  (string  def *> whitespace1) -- "def"
           *> liftA2 (,)                -- (,) is the tuple constructor: (,) a b = (a,b)
           (word' <* whitespace1)       -- name of the word
           definitionBody               -- { Stabl* }
           
-- | Parser for declarations in a file. There has to be at least one declaration 
declarations :: Parser [WordDef]
declarations = wordDef `sepBy` whitespace1

parseDecl =  parse declarations 

quotation :: Parser Quot
quotation = between (string openQuot) (string closeQuot) sequenceStablToken

-- | The same syntax as a quotation, only the def is enclosed by curly braces instead of square braces.
definitionBody :: Parser Quot
definitionBody = between (string openDef) (string closeDef) sequenceStablToken

-- | The usual integer
int :: Parser Stabl
int =     fmap (Lit . read) (many1 digit)
       <* notFollowedBy letter


-- | called "word prime" in order to be sure that it doesn't conflict
-- | with any function named "word" in Parsec.
word' :: Parser Word
word' = many1 alphaNum

wordCall :: Parser Stabl
wordCall = fmap WordCall word' 

-- | A token that can't be parsed as an int literal is assumed to be a word. If a token can't be parsed as an int, the lookahead fails without 1. consuming any input 2. propagating an error.  
stablToken :: Parser Stabl
stablToken =  (try int) <|> wordCall <|> (fmap Quotation quotation)

sequenceStablToken :: Parser [Stabl]
sequenceStablToken =    whitespace         
                     *> (stablToken `sepBy` whitespace1)
                     <* whitespace -- BUG: gives exception if there is whitespace at end of string 

whitespace :: Parser String
whitespace = many space

whitespace1 :: Parser String
whitespace1 = many1 space

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = parse sequenceStablToken

parseStablUnsafely :: String -> [Stabl]
parseStablUnsafely str = case parseStabl "" str of Right expr -> expr