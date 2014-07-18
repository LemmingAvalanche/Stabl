module Parser 
       (
         Stabl(..)
       , Quot
       , parseStabl
       , parseStablUnsafely
       ) where

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Text.Parsec.Token as Token
import Control.Applicative hiding (optional)

-- def: removed as part of the grammar

openQuot = "["
closeQuot = "]"

-- syntax sugar for collections
-- TODO: implement
-- NOTE: the only collection used should just be (homogenous?) list, for the time being
openCollection = "("
closeCollection = ")"
-- Example: '(1 2 3 4)' should translate into '[1 2 3 4] list' (for now: might want to be polymorphic in the collection type).

-- syntax sugar for tuples
-- TODO: implement
openTuple = "{"
closeTuple = "}" -- todo: change to somwething like < > instead...
-- Example: '{1 2 "boat" 4}' should translate into '[1 2 "boat" 4] tuple'. (assuming that "boat" is a string in the language).

-- TODO: implement
-- TODO: should be able to nest?
-- IDEA: have '/' built-in to the grammar of this as a an escape character? That way you can nest these things, or use « in it as a literal by using \«, as you please.
openMinceQuote = "«"
closeMinceQuote = "»"

type Word = String
-- quotation. see: cat-lang
-- NOTE: can use Quot as a collection/list: don't need a separate collection/list syntax and semantics
-- (should probably have some primitive functions for this, like cons and append).
type Quot = [Stabl] 

data Stabl = WordCall Word
           | Quotation Quot
           | LitInt Integer
           | LitChar Char 
           | Term String 
                 deriving (Read,Eq,Ord) 
                          
instance Show Stabl where
  show (WordCall w) = w
  show (Quotation q) = show q
  show (LitInt i) = show i
  show (LitChar c) = show c                      
  

-- IDE: Clojure bruker hashtag i syntaksen for å lage et sett: #{"bla" "blabla"}
-- Dette kan også vere ein god ide viss eg har lyst å lage andre datatyper enn dei eg allereie har: eg kan la 
-- ein lage "konstruktører" ved å prefikse "{" med det ein vil, for eksempel "set{"bla" "blabla"} eller "map{("hei" 2) ("hade" 3)}
-- og det viktigaste med dette er at eg trur eg kan inkludere dette i grammatikken utan at det gjer den meir kompleks eller ambigiøs!!


quotation :: Parser Quot
quotation = between (string openQuot) (string closeQuot) sequenceStablToken

tuple :: Parser Quot
tuple = between (string openTuple) (string closeTuple) sequenceStablToken

collection :: Parser Quot
collection = between (string openCollection) (string openCollection) sequenceStablToken

tick = '\''

(<:>) = liftA2 (:)

-- | The usual integer
-- See: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec#parsing-integers-with-leading-sign
int :: Parser Stabl
int = LitInt . read <$> (minus <|> number)
           where minus  = char '-' <:> number 
                 number = many1 digit
                          <* notFollowedBy letter 

-- Just a regular Char: 'e', for example
char' :: Parser Stabl
char' = LitChar <$> (charDelimiter *> anyChar <* charDelimiter)
  where charDelimiter = char tick

-- | called "word prime" in order to be sure that it doesn't conflict
-- | with any function named "word" in Parsec.
word' :: Parser Word
word' = many1 alphaNum <* (notFollowedBy $ char tick)

wordCall :: Parser Stabl
wordCall = WordCall <$> word' 

-- TODO: perhaps make the parser also accept strings that start with ':'. 
term :: Parser Stabl -- forandre til Parser Stabl
term = Term <$> (upper <:> word') -- Note: 'upper' only parses upper ASCII characters. 
            

-- | A token that can't be parsed as an int literal is assumed to be a word. If a token can't be parsed as an int, the lookahead fails without 1. consuming any input 2. propagating an error. 
stablToken :: Parser Stabl
stablToken =      try int
              <|> char' 
              <|> term
              <|> wordCall
              <|> Quotation <$> quotation -- OBS: la til char' nylig: ikkje testa med denne lagt til

sequenceStablToken :: Parser [Stabl]
sequenceStablToken =    whitespace         
                     *> stablToken `sepBy` whitespace1
                     <* whitespace -- BUG: gives exception if there is whitespace at end of string 

whitespace :: Parser String
whitespace = many space

whitespace1 :: Parser String
whitespace1 = many1 space

parseStabl :: SourceName -> String -> Either ParseError [Stabl]
parseStabl = parse sequenceStablToken

parseStablUnsafely :: String -> [Stabl]
parseStablUnsafely str = case parseStabl "" str of Right expr -> expr
