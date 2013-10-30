import Text.ParserCombinators.Parsec -- AST for the language. This should really only be the list of legal tokens of the language. 
import Control.Monad hiding ((<|>))

data Stabl = Word String    -- Word
               | Lit Int    -- Literal
                 deriving (Show,Read,Eq) -- TODO: meir?

-- Tester Parsec.......

-- In the current implementation, a digit HAS TO be followed by some whitespace. This should be fixed so that it has to be followed by some whitespace, or by end of input.
int :: Parser Stabl
int =  do
       d <- liftM (Lit . read) $ many1 digit
       _ <- skipMany1 space 
       return d

-- | Try parsing an int. If the parse fails, no input is consumed and no error is propagated. 
tryInt :: Parser Stabl
tryInt = try $ lookAhead int       

word :: Parser Stabl
word = liftM Word $ many1 alphaNum -- TODO: change to any character that is not whitespace?

stablToken :: Parser Stabl
stablToken =  tryInt <|> word -- a token that can't be parsed as an int literal is assumed to be a word

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = do 
  program <- parse $ sepBy stablToken spaces 
  _ <- parse $ skipMany space :: SourceName -> String -> Either ParseError () -- Discard all whitespace at end of program
  return program