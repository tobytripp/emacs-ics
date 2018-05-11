-- -*- coding: utf-8 -*-
module EmacsDiary.Parser (diary, record, entry) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified EmacsDiary.Record as Rec
import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as I

diary :: Parser [Rec.Record]
diary = manyTill record eof

-- TODO: unwind/explain the (<$>) and (<*>) operators

record :: Parser Rec.Record
record = do
  r <- Rec.empty <$> day
  let entryP = entry r
  entries <- (Tok.lexeme $ many entryP) <?> "Record"
  return $ foldr Rec.push r entries

day :: Parser Rec.Day
day = Rec.Singular <$> I.date <?> "Day"

entry :: Rec.Record -> Parser Rec.Entry
entry (Rec.Record (Rec.Singular d) _) =
  Rec.Entry <$> I.interval d <*> fields <?> "Record.Entry"


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
