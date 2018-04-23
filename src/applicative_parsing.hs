module ApplicativeParsing where
import Text.Parsec (ParseError, parse, many1, chainl1, eof, letter, satisfy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)


data SimpleExpr = Num Integer
                | Var String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                deriving (Eq,Show)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""


myParser1 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
myParser1 ctor pa pb = do
  a <- pa
  b <- pb
  return $ ctor a b

-- Minus “do” sugar
myParser2 ctor pa pb =
  pa >>= \a -> pb >>= \b -> return $ ctor a b

-- or as:
myParser3 ctor pa pb = ctor `fmap` pa `ap` pb

-- (This uses functions from Applicative instead of Monad.) We replace the use
-- of >>= with fmap and ap. This isn’t always possible, but it often is.

-- Using Applicative’s operators
myParser4 ctor pa pb = ctor <$> pa <*> pb

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexemeAO :: Parser a -> Parser a
lexemeAO p = do
  x <- p <* whitespace
  return x

-----------------------------------------------------------------
-- 4.1 lexeme http://jakewheat.github.io/intro_to_parsing/#_lexeme
-- Refactoring to Applicative

lexemeA0 :: Parser a -> Parser a
lexemeA0 p = do
  x <- p <* whitespace
  return x

-- -- Roughly:
-- (<*) :: Parser a -> Parser b -> Parser a
-- (<*) pa pb = do
--     a <- pa
--     void pb
--     return a
--
-- -- (It isn’t implemented this way, since (<*) only needs Applicative and
-- -- not Monad.)

lexemeA1 p = do
  p <* whitespace

lexemeA :: Parser a -> Parser a
lexemeA p = p <* whitespace

-- ‘D` for "do"
numD :: Parser SimpleExpr
numD = do
  n <- lexemeA0 $ many1 digit
  return $ Num $ read n

numA0 :: Parser SimpleExpr
numA0 = do
  n <- read <$> lexemeA (many1 digit)
  return $ Num n

numA1 = do
  n <- (Num . read) <$> lexemeA (many1 digit)
  return n

numA2 = do
  n <- Num <$> read <$> lexemeA (many1 digit)
  return n

numA2'' = do
  n <- numb
  return n
  where
    numb :: Parser SimpleExpr
    numb = Num <$> int
    int :: Parser Integer
    int = read <$> lexemeA (many1 digit)

numA3 = (Num . read) <$> lexemeA (many1 digit)

-- “In more 'industrial' parser code, I would usually write some tokenization
-- parsers separately like this:”
--
integerA4 :: Parser Integer
integerA4 = read <$> lexemeA (many1 digit)

-- Then the num expression parser looks like this:
--
numA4 :: Parser SimpleExpr
numA4 = Num <$> integerA4

-- Here is the previous var parser:

varD :: Parser SimpleExpr
varD = lexemeA $ do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

varA0 :: Parser SimpleExpr
varA0 = lexemeA $ do
  f1 <- firstChar
  rest <- many nonFirstChar
  return $ Var (f1:rest)
  where
    firstChar = letter <|> char '+'
    nonFirstChar = digit <|> firstChar

-- We can lift the (:) using the Applicative operators.
--
varA1 :: Parser SimpleExpr
varA1 = do
    i <- iden
    return $ Var i
  where
    iden = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

varA2 = Var <$> iden
  where
    iden = lexemeA ((:) <$> firstChar <*> many nonFirstChar)
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

------------------------------------------------------------------
-- 4.4 parens

parensD :: Parser SimpleExpr
parensD = do
    void $ lexemeA $ char '('
    e <- simpleExprD
    void $ lexemeA $ char ')'
    return $ Parens e

-- Here is the rewrite in one step:
--
parensA0 :: Parser SimpleExpr
parensA0 =
    Parens <$> (lexemeA (char '(')
                *> simpleExprD
                <* lexemeA (char ')'))


termD :: Parser SimpleExpr
termD = numD <|> varD <|> parensD

simpleExprD = chainl1 termD op
  where
    op = do
      void $ lexemeA $ char '+'
      return Add

simpleExprA0 = chainl1 termD op
  where op = lexemeA (char '+') *> return Add

simpleExprA1 = chainl1 termD op
  where op = Add <$ lexemeA (char '+')

simpleExprA2 = chainl1 termD (Add <$ lexemeA (char '+'))
