-- -*- coding: utf-8 -*-
module EmacsDiary.Parser (
  diary,                        -- ^ parse an Emacs Diary

  module EmacsDiary.Parser.Record
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Time.LocalTime (ZonedTime)

import EmacsDiary.Parser.Record (record, entry)
import qualified EmacsDiary.Record as Rec

diary  :: ZonedTime -> Parser [Rec.Record]
diary localt = manyTill (record localt) eof
