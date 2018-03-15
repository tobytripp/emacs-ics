module EmacsDiary.Parser.Tokens where

import Text.Parsec (many1)
import Text.Parsec.Char (letter)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer    = P.makeTokenParser emptyDef
numeric  = P.natural lexer
symbol s = P.symbol lexer s

word :: Parser String
word = P.lexeme lexer $ many1 letter
