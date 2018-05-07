module EmacsDiary.Parser.Tokens where

import Text.Parsec (Column, sepBy, chainl, (<?>), manyTill, (<|>), many, many1, oneOf, noneOf, choice, try, many1, count)
import Text.Parsec.Char (letter, string, space, digit, anyChar, endOfLine)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (LanguageDef(..), emptyDef)

lexer      = P.makeTokenParser diaryDef
numeric    = P.natural lexer
symbol     = P.symbol lexer
lexeme     = P.lexeme lexer
whitespace = P.whiteSpace lexer

indent :: Column -> Parser String
indent i = do
  ss <- P.lexeme lexer $ count i space
  return ""

digits :: Parser String
digits = P.lexeme lexer $ many1 digit

word :: Parser String
word = P.lexeme lexer $ many1 letter

line :: Parser String
line = many1 (noneOf ";\n\r")

blanklines :: Parser String
blanklines = do {whitespace; return ""}

keyword kw = P.symbol lexer kw <?> kw

diaryDef :: LanguageDef st
diaryDef = emptyDef
           { P.commentLine    = "#"
           , P.caseSensitive  = True
           }
