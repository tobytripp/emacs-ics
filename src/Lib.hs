module Lib (parseDiary, now, toIcs) where

import Control.Monad (liftM)

import Text.Parsec (runParser)
import Text.Parsec.String (Parser)
import Text.Printf

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (
  ZonedTime(..),
  getCurrentTimeZone,
  utcToLocalTime)

import EmacsDiary.Parser (diary, Record)
import EmacsDiary.Record (Diary(..))
import EmacsDiary.Ics (toIcs)

-- now :: IO ZonedTime
now = do
  tz  <- getCurrentTimeZone
  utc <- getCurrentTime
  let local = utcToLocalTime tz utc
  return $ ZonedTime local tz


parseDiary :: ZonedTime -> FilePath -> IO Diary
parseDiary t path = do
  input  <- readFile path
  case runParser (diary t) () path input of
    (Left e)  -> return $ Failed (printf "Parse error: '%s'" (show e))
    (Right d) -> return $ d
