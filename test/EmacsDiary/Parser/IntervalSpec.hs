module EmacsDiary.Parser.IntervalSpec
        ( intervalTests
        ) where

import Test.HUnit

import EmacsDiary.Parser.Interval
import SpecHelpers

intervalTests = test [
  "parse time on given day" ~: do
    let d = parseDate "5 May 2018"
    let (Right actual) = testParse (time d) "17:18"
    " 5 May 2018 17:18" @=? showTime actual
    ,
  "parsing valid date" ~: do
    let input = "5 May 2018"
    let expected = parseDate input
    let (Right actual) = testParse date input
    " 5 May 2018 00:00" @=? showTime actual
  --   ,
  -- "using assertion" ~: do
  --   let input = "5 May 2018"
  --   assertParsesTo " 5 May 2018 00:00" date input
  ]
