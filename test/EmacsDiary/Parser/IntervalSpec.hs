-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec (tests) where

import Test.HUnit

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (hoursToTimeZone)

import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser.Interval
import SpecHelpers

epoch   = I.epoch
cdtdate = I.fromNumbers cdt
utcdate = I.fromNumbers I.utc

dateParser = date cdt

tests = test [
  "parse basic date" ~: do
      let input = "4 May 2020"
      let expected = cdtdate 4 5 2020
      assertParsesTo (date cdt) input expected
  ,
  "parse long month" ~: do
      let input = "04 January 2020"
      let expected = cdtdate 4 1 2020
      assertParsesTo dateParser input expected
  ,
  "parsing week-day" ~: do
      let input = "Thursday 14:30 Coffee"
      let expected = I.DayOfWeek I.Thursday cdt
      assertParsesTo dateParser input expected
      ,
  "parsing T’s birthday" ~: do
      let input = "7 July 2008"
      let expected = cdtdate 7 7 2008
      assertParsesTo dateParser input expected
  ,

  "parsing time" ~: do
      let input = "17:00"
      let expected = I.timeOn epoch 17 00
      assertParsesTo (time epoch) input expected
  ,

  "parsing time with leading whitespace" ~: do
      let input = "  17:00"
      let expected = I.timeOn epoch 17 00
      assertParsesTo (time epoch) input expected
  ,

  "parsing time on given date" ~: do
      let d = cdtdate 7 7 2008
      let expected = I.timeOn d 17 30
      let input = "17:30"
      assertParsesTo (time d) input expected
      ,

  "parsing a time-range" ~: do
      let input = "  11:00-12:00"
      let expected = I.interval (I.timeOn epoch 11 00) (Just $ I.timeOn epoch 12 00)
      assertParsesTo (interval epoch) input expected
  ,

  "time-interval with no specified ending" ~: do
      let input = "  11:35"
      let expected = I.instant epoch 11 35
      assertParsesTo(interval epoch) input expected
  ]

