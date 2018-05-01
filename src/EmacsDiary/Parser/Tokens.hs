module EmacsDiary.Parser.Tokens where

import Text.Parsec ((<?>), manyTill, (<|>), many, many1, oneOf, noneOf, choice, try, many1, count)
import Text.Parsec.Char (letter, string, space, digit, anyChar, endOfLine)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer      = P.makeTokenParser emptyDef
numeric    = P.natural lexer
symbol     = P.symbol lexer
lexeme     = P.lexeme lexer
whitespace = P.whiteSpace lexer

digits :: Parser String
digits = P.lexeme lexer $ many1 digit

word :: Parser String
word = P.lexeme lexer $ many1 letter

line :: Parser String
line = many1 (noneOf "\n\r") <* endOfLine

blankline :: Parser String
blankline = do
  _ <- manyTill (oneOf " \t") endOfLine
  return ""

keyword kw = P.symbol lexer kw <?> kw
