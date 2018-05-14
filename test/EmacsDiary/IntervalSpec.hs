-- -*- coding: utf-8 -*-
module EmacsDiary.IntervalSpec (tests) where

import Test.HUnit
import SpecHelpers

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime

import EmacsDiary.Interval

zonedDate y m d =  Date (fromGregorian y m d) tz
  where
    tz             = hoursToTimeZone (-5)

zonedTime y m d hr mn =
  zoned
  where
    d@(Date day _) = date 7 7 2008
    tz             = hoursToTimeZone (-5)
    localTime      = LocalTime day (TimeOfDay hr mn 00)
    zoned          = ZonedTime localTime tz
    UTCTime _ utc  = zonedTimeToUTC zoned


tests = test [
  "in `timeOn` the given date is added to specified time" ~: do
      let d = date 7 7 2008
      let (Time actual) = timeOn d 14 30
      let (Just expected) =
            parseTimeM True defaultTimeLocale
              "%FT%R" "2008-07-07T14:30" :: Maybe UTCTime
      assertEqual "" expected actual
  ,
  "local time-zone is converted to UTC" ~: do
      let d@(Date day _) = zonedDate 7 7 2008
      let expected       = UTCTime day 70200
      let (Time actual)  = timeOn d 14 30

      assertEqual "" expected actual
  ]
