-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.IntervalSpec
        ( intervalTests
        ) where

import Test.HUnit
import Data.Time.Clock (utctDay, UTCTime)

import EmacsDiary.Parser.Interval
import SpecHelpers

data DayMonth = DayMonth   { d :: Integer, dm :: Integer } deriving (Eq, Show)
data MonthYear = MonthYear { md :: Integer, y :: Integer } deriving (Eq)
instance Show MonthYear where
  show my = (show $ md my) ++ "-" ++ (show $ y my)

assertEither :: Show a => Either String a -> String
assertEither e = case e of
  (Left  error)  -> error
  (Right actual) -> show actual

intervalTests = test [
  "parse basic date" ~: do
      let expected = assertEither $ parseDateF "%d %b %Y" "04 May 2020"
      case testParse date "basic date" "4 May 2020" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" (show actual) expected
  ,
  "parse long month" ~: do
      let expected = assertEither $ parseDateF "%d %B %Y" "04 January 2020"
      case testParse date "long month" "04 January 2020" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" (show actual) expected
  ,
  "parsing T’s birthday" ~: do
      let expected = assertEither $ parseDateF "%d %B %Y" "07 July 2008"
      case testParse date "birthday" "7 July 2008" of
        (Left error)   -> assertFailure (show error)
        (Right actual) -> assertEqual "" (show actual) expected
  ]
