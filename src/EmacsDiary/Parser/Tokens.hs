module EmacsDiary.Parser.Tokens where

import Text.Parsec (many, choice, try, many1, count)
import Text.Parsec.Char (letter, string)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer    = P.makeTokenParser emptyDef
numeric  = P.natural lexer
symbol s = P.symbol lexer s

word :: Parser String
word = P.lexeme lexer $ many1 letter

keyValueParser :: (String, Integer) -> Parser Integer
keyValueParser (s, v) =
  try (string s) <*
  (P.lexeme lexer $ many letter) >>
  return v

mappingParser :: [(String, Integer)] -> Parser Integer
mappingParser = choice . map keyValueParser

monthOrdinal =
  mappingParser [
  ("Jan",  1),
  ("Feb",  2),
  ("Mar",  3),
  ("Apr",  4),
  ("May",  5),
  ("Jun",  6),
  ("Jul",  7),
  ("Aug",  8),
  ("Sep",  9),
  ("Oct", 10),
  ("Nov", 11),
  ("Dec", 12)
  ]
