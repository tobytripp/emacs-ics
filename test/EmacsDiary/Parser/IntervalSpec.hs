-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec (tests) where

import Test.HUnit

import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser.Interval
import SpecHelpers

epoch = I.epoch

tests = test [
  "parse basic date" ~: do
      let (Right expected) = parseDateF "%d %b %Y" "04 May 2020"
      case testParse date "basic date" "4 May 2020" of
        (Left error)   -> assertFailure (show error)
        (Right (I.Date actual _)) -> assertEqual "" expected actual
  ,
  "parse long month" ~: do
      let (Right expected) = parseDateF "%d %B %Y" "04 January 2020"
      case testParse date "long month" "04 January 2020" of
        (Left error)   -> assertFailure (show error)
        (Right (I.Date actual _)) -> assertEqual "" expected actual
  ,
  "parsing T’s birthday" ~: do
      let (Right expected) = parseDateF "%d %B %Y" "07 July 2008"
      case testParse date "birthday" "7 July 2008" of
        (Left error)   -> assertFailure (show error)
        (Right (I.Date actual _)) -> assertEqual "" expected actual
  ,

  "parsing time" ~: do
      let (Right expected) = parseTimeF "%d/%m/%Y %H%M" "01/01/1970 1700"
      case testParse (time epoch) "simple time" "17:00" of
        (Left  error)  -> assertFailure (show error)
        (Right (I.Time actual)) -> assertEqual "" expected actual
  ,

  "parsing time with leading whitespace" ~: do
      let (Right expected) = parseTimeF "%d/%m/%Y %H%M" "01/01/1970 1700"
      case testParse (time epoch) "ws time" "  17:00" of
        (Left  error)  -> assertFailure (show error)
        (Right (I.Time actual)) -> assertEqual "" expected actual
  ,

  "parsing time on given date" ~: do
      let d = I.date 7 7 2008
      let (Right expected) = parseTimeF "%d/%m/%Y %H%M" "07/07/2008 1730"
      let input = "17:30"
      case testParse (time d) input input of
        (Left  error)  -> assertFailure (show error)
        (Right (I.Time actual)) -> assertEqual "" expected actual
      ,

  "parsing a time-range" ~: do
      let input = "  11:00-12:00"
      case testParse (interval epoch) input input of
        (Left error)   -> assertFailure $ show error
        (Right actual) -> assertEqual ""
          "19700101T11:00-19700101T12:00 UTC" (show actual)
  ,

  "time-interval with no specified ending" ~: do
      let input = "  11:35"
      case testParse (interval epoch) input input of
        (Left error)   -> assertFailure $ show error
        (Right actual) -> assertEqual ""
          "19700101T11:35 UTC" (show actual)
  ]
