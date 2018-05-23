-- -*- coding: utf-8 -*-
module EmacsDiary.Parser (
  diary,                        -- ^ parse an Emacs Diary

  module EmacsDiary.Parser.Record,
  module EmacsDiary.Record
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Time.LocalTime (ZonedTime)

import EmacsDiary.Parser.Record (record, entry)
import EmacsDiary.Record (Diary(..), Record)

diary  :: ZonedTime -> Parser Diary
diary localt = Diary <$> manyTill (record localt) eof
