-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec
        ( intervalTests
        ) where

import Test.HUnit

import Data.Time.Format (formatTime, defaultTimeLocale)

import EmacsDiary.Parser.Interval
import SpecHelpers

assertEither :: Show a => Either String a -> String
assertEither e = case e of
  (Left  error)  -> error
  (Right result) -> show result

intervalTests = test [
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
  ]
