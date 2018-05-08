-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec (tests) where

import Test.HUnit

import Data.Time.Format (formatTime, defaultTimeLocale)

import EmacsDiary.Interval (Date(..), Time(..))
import EmacsDiary.Parser.Interval
import SpecHelpers

data DateTime = DateTime {d :: Date, t :: Time} deriving (Eq)
instance Show DateTime where
  show (DateTime d t) = unwords [(show d), "/", (show t)]

assertEither :: Show a => Either String a -> String
assertEither e = case e of
  (Left  error)  -> error
  (Right result) -> show result

tests = test [
  "parse basic date" ~: do
      let expected = assertEither $ parseDateF "%d %b %Y" "04 May 2020"
      case testParse date "basic date" "4 May 2020" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,
  "parse long month" ~: do
      let expected = assertEither $ parseDateF "%d %B %Y" "04 January 2020"
      case testParse date "long month" "04 January 2020" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,
  "parsing T’s birthday" ~: do
      let expected = assertEither $ parseDateF "%d %B %Y" "07 July 2008"
      case testParse date "birthday" "7 July 2008" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,

  "parsing time" ~: do
      let expected = assertEither $ parseTimeF "%d/%m/%Y %H%M" "01/01/1970 1700"
      case testParse time "simple time" "17:00" of
        (Left  error)  -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,

  "parsing time with leading whitespace" ~: do
      let expected = assertEither $ parseTimeF "%d/%m/%Y %H%M" "01/01/1970 1700"
      case testParse time "ws time" "  17:00" of
        (Left  error)  -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,

  "parse date and time" ~: do
      let input = "7 July 2008 14:30"
      let expected = "2008-07-07 / 1970-01-01 14:30:00 UTC"
      case testParse (DateTime <$> date <*> time) "date-time" input of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" expected (show actual)
  ,

  "parsing a time-range" ~: do
      let input = "  11:00-12:00"
      case testParse interval input input of
        (Left error)   -> assertFailure $ show error
        (Right actual) -> assertEqual "" "11:00-12:00 UTC" (show actual)
  ,

  "time-interval with no specified ending" ~: do
      let input = "  11:35"
      case testParse interval input input of
        (Left error)   -> assertFailure $ show error
        (Right actual) -> assertEqual "" "11:35 UTC" (show actual)
  ]
