-- -*- coding: utf-8 -*-
module EmacsDiary.Parser (
  diary,                        -- ^ parse an Emacs Diary

  module EmacsDiary.Parser.Record
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Time.LocalTime (TimeZone)

import EmacsDiary.Parser.Record (record, entry)
import qualified EmacsDiary.Record as Rec

diary  :: TimeZone -> Parser [Rec.Record]
diary tz = manyTill (record tz) eof
