module EmacsDiary.Parser (diary, entry) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified EmacsDiary.Record as Rec
import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as I

description :: Parser Rec.Description
description = Tok.phrase

location :: Parser Rec.Address
location = Tok.phrase

entry :: Parser Rec.Entry
entry = Rec.Entry <$> I.time <*> description <*> location

record :: Parser Rec.Record
record = Rec.Record <$> I.date <*> many entry

diary = many1 record
