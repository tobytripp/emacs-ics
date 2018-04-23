module EmacsDiary.Parser.Tokens where

import Text.Parsec ((<|>), many, choice, try, many1, count)
import Text.Parsec.Char (letter, string, digit)
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

dateToken = digits <|> word
