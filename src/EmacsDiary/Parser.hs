module EmacsDiary.Parser (diary) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

import EmacsDiary.Parser.Interval

diary = date
