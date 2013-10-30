import Text.ParserCombinators.Parsec -- AST for the language. This should really only be the list of legal tokens of the language. 
import Control.Monad

data Stabl = Word String    -- Word
               | Lit Int    -- Literal
                 deriving (Show,Read,Eq) -- TODO: meir?

-- Tester Parsec.......

int :: Parser Stabl
int = liftM (Lit . read) $ many1 digit

word :: Parser Stabl
word = liftM Word $ many1 alphaNum -- TODO: change to any character that is not whitespace?

stablToken :: Parser Stabl
stablToken = int <|> word -- a token that can't be parsed as an int literal is assumed to be a word

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = do 
  program <- parse $ sepBy stablToken spaces 
  _ <- parse $ many1 space :: SourceName -> String -> Either ParseError String-- Discard all whitespace at end of program
  return program