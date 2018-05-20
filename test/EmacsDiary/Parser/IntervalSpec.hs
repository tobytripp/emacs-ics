-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec (tests) where

import Test.HUnit

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (hoursToTimeZone)

import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser.Interval
import SpecHelpers

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

tstamp = ZonedTime local cdt
  where
    local = LocalTime d t
    d     = fromGregorian 2008 07 07
    t     = TimeOfDay 12 00 00
cdtdate y m d = I.utcDate tstamp (I.gregorian y m d)

dateParser = date tstamp

tests = test [
  "parse basic date" ~: do
      let input = "4 May 2020"
      let expected = cdtdate 2020 5 4
      assertParsesTo dateParser input expected
  ,
  "parse long month" ~: do
      let input = "04 January 2020"
      let expected = cdtdate 2020 1 4
      assertParsesTo dateParser input expected
  ,
  "parsing week-day" ~: do
      let input = "Thursday 14:30 Coffee"
      let expected = I.DayOfWeek I.Thursday tstamp
      assertParsesTo dateParser input expected
      ,
  "parsing T’s birthday" ~: do
      let input = "7 July 2008"
      let expected = cdtdate 2008 7 7
      assertParsesTo dateParser input expected
  ,

  "parsing time" ~: do
      let input = "17:00"
      let expected = I.makeTime I.epoch 22 00
      assertParsesTo (time (cdtdate 1970 1 1)) input expected
  ,

  "parsing time with leading whitespace" ~: do
      let input = "  17:00"
      let expected = I.makeTime I.epoch 22 00
      assertParsesTo (time (cdtdate 1970 1 1)) input expected
  ,

  "parsing time on given date" ~: do
      let d@(I.Date day _) = cdtdate 2008 7 7
      let input = "17:30"
      let seconds =   22 * 60 * 60
                    + 30 * 60
      let expected = I.Time d (UTCTime day (secondsToDiffTime seconds))
      assertParsesTo (time d) input expected
      ,

  "parsing a time-range" ~: do
      let input = "  11:00-12:00"
      let expected =
            I.interval (I.makeTime I.epoch 11 00)
              (Just $ I.makeTime I.epoch 12 00)
      assertParsesTo (interval I.epoch) input expected
  ,

  "time-interval with no specified ending" ~: do
      let input = "  11:35"
      let expected = I.instant I.epoch 11 35
      assertParsesTo(interval I.epoch) input expected
  ]

