module EmacsDiary.Parser (diary, entry) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified EmacsDiary.Record as Rec
import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as I

description :: Parser Rec.EntryField
description = Rec.Description <$>
  Tok.line <?> "description"

location :: Parser Rec.EntryField
location = Rec.Location <$>
  (Tok.whitespace *>
  Tok.keyword "Location:" *>
  Tok.line <?> "location")

field = description <|> location

entry :: Parser Rec.Entry
entry = Rec.Entry <$>
  I.time <*>
  many field

record :: Parser Rec.Record
record = Rec.Record <$> I.date <*> many entry

diary = manyTill record eof
