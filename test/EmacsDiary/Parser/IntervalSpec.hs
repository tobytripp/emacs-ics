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
  --  ,
  -- "parse time on given day" ~: do
  --     let d = parseDate "5 May 2018"
  --     let (Right actual) = testParse (time d) "time" "17:18"
  --     " 5 May 2018 17:18" @=? showTime actual
  --   ,
  -- "parsing short month" ~: do
  --       let (Right actual) = testParse month "short month" "1 Jan 2016"
  --       1 @=? actual
  --   ,
  -- "parse long month" ~: do
  --       let (Right actual) = testParse month "long-month" "4 January 2017"
  --       1 @=? actual
  --   ,
  -- "month parsing consumes the entire word" ~: do
  --       assertParser "month parse fail"
  --         "1-2018"
  --         (MonthYear <$> month <*> year)
  --         "January 2018"
  -- "parsing valid date" ~: do
  --       let input = "5 May 2018"
  --       let expected = parseDate input
  --       let (Right actual) = testParse date input
  --       " 5 May 2018 00:00" @=? showTime actual
  --   ,
  -- "parsing the day" ~: do
  --       let input    = "7 July 2008"
  --       let expected = 7
  --       let (Right actual) = testParse day input
  --       7 @=? actual
  --   ,
  -- "parsing the year" ~: do
  --       let input    = "2008"
  --       let (Right actual) = testParse year input
  --       2008 @=? actual
  --   ,
  -- "parsing day and month" ~: do
  --       let input    = "7 July 2008"
  --       let expected = DayMonth 7 7
  --       let (Right actual) = testParse (DayMonth <$> day <*> month) input
  --       expected @=? actual
  --   ,
  -- "parsing T’s birthday" ~: do
  --       let input    = "7 July 2008"
  --       let expected = Date 7 7 2008
  --       case testParse dateS input of
  --         (Right actual) ->  expected @=? actual
  --         (Left error) -> assertFailure (show error)
  ]
