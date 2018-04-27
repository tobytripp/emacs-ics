module EmacsDiary.Parser.Tokens where

import Text.Parsec (manyTill, (<|>), many, choice, try, many1, count)
import Text.Parsec.Char (letter, string, digit, endOfLine)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer   = P.makeTokenParser emptyDef
numeric = P.natural lexer
symbol  = P.symbol lexer
lexeme  = P.lexeme lexer

digits :: Parser String
digits = P.lexeme lexer $ many1 digit

word :: Parser String
word = P.lexeme lexer $ many1 letter

phrase :: Parser String
phrase = unwords <$> manyTill word endOfLine
