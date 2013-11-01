import Text.ParserCombinators.Parsec -- AST for the language. This should really only be the list of legal tokens of the language. 
import Control.Monad hiding ((<|>))

data Stabl = Word String    -- Word
               | Lit Int    -- Literal
                 deriving (Show,Read,Eq) -- TODO: meir?

-- | The usual integer
int :: Parser Stabl
int =  do 
  i <- liftM (Lit . read) $ many1 digit 
  _ <- notFollowedBy letter -- TODO: change? a string like "225+-" shouldn't be parsed by consuming "225" and discarding "+-". 
  return i

-- | called "word prime" in order to be sure that it doesn't conflict
-- | with any function named "word" in Parsec.
word' :: Parser Stabl
word' = liftM Word $ many1 alphaNum -- TODO: change to any character that is not whitespace? It should be possible to use strings like "+" and "-" as identifiers of words. 

-- | A token that can't be parsed as an int literal is assumed to be a word. If a token can't be parsed as an int, the lookahead fails without 1. consuming any input 2. propagating an error.  
stablToken :: Parser Stabl
stablToken =  (try int) <|> word'

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = do
  _ <- discardWhitespace
  program <- parse $ stablToken `sepBy` spaces 
  _ <- discardWhitespace  -- Discard all whitespace at end of program
  return program 
    -- TODO: found a bug: this doesn't seem to help with whitespace at start of input (returns empty list of tokens) nor at end of input (throws an exception). 
    where discardWhitespace = parse $ many space :: SourceName -> String -> Either ParseError String

