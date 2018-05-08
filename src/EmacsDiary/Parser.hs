module EmacsDiary.Parser (diary, record, entry) where

import Data.Char (isSpace)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import Control.Monad (void, ap)

import qualified EmacsDiary.Record as Rec
import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as I

diary :: Parser [Rec.Record]
diary = manyTill record eof


record :: Parser Rec.Record
record = Rec.Record <$> I.date <*> (Tok.lexeme $ many entry) <?> "Record"

entry :: Parser Rec.Entry
entry = Rec.Entry <$> I.interval <*> fields <?> "Record.Entry"


fields :: Parser [Rec.EntryField]
fields = Tok.lexeme $ sepBy1 field sep
  where
    field = (description <|> location) <?> "entry field"
    sep = try (endOfLine *> Tok.indent 4)
      <|> (Tok.symbol ";")
      <?> "field-separator"


description = Rec.Description <$>
  (Tok.line <?> "description")

location = Rec.Location <$>
  (Tok.keyword "Location:" *>
   Tok.line <?> "location")
