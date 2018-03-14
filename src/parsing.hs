module Parsing where
import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

doParse :: Parser a -> String -> Either ParseError a
doParse p = parse p "testing"


num :: Parser Integer
num = do
  n <- many1 digit
  return (read n)

num' :: Parser Integer
num' = do
  n <- many digit
  return (read n)

var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

data Parenthesis = Parentheses Integer
                 deriving (Eq, Show)

parens :: Parser Parenthesis
parens = do
  _ <- char '('
  e <- many1 digit
  _ <- char ')'
  return (Parentheses (read e))

data SingleAdd = SingleAdd Integer Integer
               deriving (Eq, Show)

add :: Parser SingleAdd
add = do
  e0 <- many1 digit
  _ <- char '+'
  e1 <- many1 digit
  return (SingleAdd (read e0) (read e1))

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parensW :: Parser Parenthesis
parensW = do
  whitespace
  _ <- char '('
  whitespace
  e <- many1 digit
  whitespace
  _ <- char ')'
  whitespace
  return (Parentheses (read e))

-- ’lexeme' parsing: token parsers consume any trailing white-space
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
      whitespace
      p

parensL :: Parser Parenthesis
parensL = do
  void $ lexeme $ char '('
  e <- lexeme $ many1 digit
  void $ lexeme $ char ')'
  return (Parentheses (read e))

parseWithWhitespace' :: Parser a -> String -> Either ParseError a
parseWithWhitespace' p = parseWithEof (whitespace >> p)

data SimpleExpr = Num Integer
                | Var String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                deriving (Eq, Show)

numE :: Parser SimpleExpr
numE = do
  n <- lexeme $ many1 digit
  return $ Num $ read n

varE :: Parser SimpleExpr
varE = lexeme $ do
  fc <- firstChar
  rest <- many nonFirstChar
  return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parensE :: Parser SimpleExpr
parensE = do
  void $ lexeme $ char '('
  e <- lexeme $ many1 digit
  void $ lexeme $ char ')'
  return $ Parens $ Num $ read e

-- reusing numE:
parensE' :: Parser SimpleExpr
parensE' = do
  void $ lexeme $ char '('
  e <- numE
  void $ lexeme $ char ')'
  return $ Parens e

addE = do
  e0 <- numE
  void $ lexeme $ char '+'
  e1 <- numE
  return $ Add e0 e1

-- choice operator
numOrVar = numE <|> varE

simpleExpr = numE <|> varE <|> addE <|> parensE

simpleExpr'  = addE <|> numE <|> varE <|> parensE
simpleExpr'' = try addE <|> numE <|> varE <|> parensE

parensEN :: Parser SimpleExpr -> Parser SimpleExpr
parensEN exprParser = do
  void $ lexeme $ char '('
  e <- exprParser
  void $ lexeme $ char ')'
  return $ Parens e

-- parser that cannot yet handle add
term :: Parser SimpleExpr -> Parser SimpleExpr
term expressionParser = numE <|> varE <|> parensEN expressionParser

term5 = term term5
addE5 = do
  e0 <- term5
  void $ lexeme $ char '+'
  e1 <- term5
  return $ Add e0 e1

simpleExpr5 = try addE5 <|> term5

-- λ> parseWithWhitespace simpleExpr5 "1+ (2+7)"
-- Left (line 1, column 2):
-- unexpected '+'
-- expecting digit or end of input

term6 = term simpleExpr6
addE6 = do
  e0 <- term6
  void $ lexeme $ char '+'
  e1 <- simpleExpr6
  return $ Add e0 e1
simpleExpr6 = try addE6 <|> term6

-- λ> parseWithWhitespace simpleExpr6 "1+ 2+7"
-- Right (Add (Num 1) (Add (Num 2) (Num 7)))

-- Oops, that’s right-associative for addition

-- “left-factor”
term7 = term simpleExpr7
simpleExpr7 = do
  -- first, parse a term
  e <- term7
  -- then see if it is followed by a ’+ expr' suffic
  maybeAddSuffix e
  where
    -- this function takes an expression, and parses a
    -- '+ expr' suffix, returning an Add expression
    -- it recursively calls itself via the maybeAddSuffix function
    addSuffix e0 = do
      void $ lexeme $ char '+'
      e1 <- term7
      maybeAddSuffix (Add e0 e1)
    -- this is the wrapper for addSuffix, which adapts it so that if
    -- addSuffix fails, it returns just the original expression
    maybeAddSuffix e = addSuffix e <|> return e

-- There is a combinator function in Parsec we can use which abstracts this
-- sort of pattern, chainl1.

simpleExpr8 :: Parser SimpleExpr
simpleExpr8 = chainl1 term8 op
  where
    op = do
        void $ lexeme $ char '+'
        return Add
    term8 = term simpleExpr8

-- the type of chainl1 is:
-- Parser a -> Parser (a -> a -> a) -> Parser a
