-- -*- coding: utf-8 -*-
module EmacsDiary.IntervalSpec (tests) where

import Test.HUnit
import SpecHelpers

import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime

import EmacsDiary.Interval

tstamp = ZonedTime local cdt
  where
    local = LocalTime d t
    d = fromGregorian 2008 07 07
    t = TimeOfDay 12 00 00

tests = test [
  "in `makeTime` the given date is added to specified date" ~: do
      let d = makeDate tstamp (2008, 7, 7)
      let (Time _ actual) = makeTime d 14 30
      let (Just expected) =
            parseTimeM True defaultTimeLocale
              "%FT%R" "2008-07-07T19:30" :: Maybe UTCTime
      assertEqual "" expected actual
  ,
  "local time-zone is converted to UTC" ~: do
      let d@(Date day _)  = makeDate tstamp (2008, 7, 7)
      let expected        = UTCTime day 70200
      let (Time _ actual) = makeTime d 14 30

      assertEqual "" expected actual
  ,
  "local time formats as UTC" ~: do
      let d@(Date day _)  = makeDate tstamp (2008, 7, 7)
      let expected        = UTCTime day 70200
      let (Time _ actual) = makeTime d 14 30

      assertEqual ""
        (formatTime defaultTimeLocale "%FT%R%Z" expected)
        (formatTime defaultTimeLocale "%FT%R%Z" actual)
  ,
  -- This would seem to imply that, on start, the parser will need the current
  -- local time so that it can find weekdays as well as convert times to UTC.
  "weekdays use the current date to find the next weekday's date" ~: do
      assertEqual "NYI" True True
  ]
