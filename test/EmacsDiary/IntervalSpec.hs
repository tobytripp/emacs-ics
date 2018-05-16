-- -*- coding: utf-8 -*-
module EmacsDiary.IntervalSpec (tests) where

import Test.HUnit
import SpecHelpers

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime

import EmacsDiary.Interval

tests = test [
  "in `timeOn` the given date is added to specified time" ~: do
      let d = fromNumbers cdt 7 7 2008
      let (Time _ actual) = timeOn d 14 30
      let (Just expected) =
            parseTimeM True defaultTimeLocale
              "%FT%R" "2008-07-07T19:30" :: Maybe UTCTime
      assertEqual "" expected actual
  ,
  "local time-zone is converted to UTC" ~: do
      let d@(Date day _)  = zonedDate 7 7 2008
      let expected        = UTCTime day 70200
      let (Time _ actual) = timeOn d 14 30

      assertEqual "" expected actual
  ,
  -- This would seem to imply that, on start, the parser will need the current
  -- local time so that it can find weekdays as well as convert times to UTC.
  "weekdays use the current date to find the next weekday's date" ~: do
      assertEqual "NYI" True True
  ]
