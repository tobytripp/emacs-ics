-- -*- coding: utf-8 -*-
module EmacsDiary.Parser (diary, record, entry) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Time.LocalTime (TimeZone)

import qualified EmacsDiary.Record as Rec
import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as I

diary  :: TimeZone -> Parser [Rec.Record]
record :: TimeZone -> Parser Rec.Record
entry  :: Rec.Record -> Parser Rec.Entry


diary tz = manyTill (record tz) eof

-- TODO: unwind/explain the (<$>) and (<*>) operators

record tz = do
  r <- Rec.empty <$> day tz
  let entryP = entry r
  entries <- (Tok.lexeme $ many entryP) <?> "Record"
  return $ foldr Rec.push r entries

entry (Rec.Record (Rec.Singular d) _) =
  Rec.Entry <$> I.interval d <*> fields <?> "Record.Entry"


day :: TimeZone -> Parser Rec.Day
day tz = Rec.Singular <$> I.date tz <?> "Day"

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
