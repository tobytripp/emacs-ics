module EmacsDiary.Parser (diary) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

import EmacsDiary.Record
import qualified EmacsDiary.Parser.Interval as I

record :: Parser Record
record = emptyRecord <$> I.date

diary = many1 record
