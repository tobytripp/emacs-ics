-- -*- coding: utf-8 -*-
module EmacsDiary.IntervalSpec (tests) where

import Test.HUnit
import SpecHelpers

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

import EmacsDiary.Interval

tests = test [
  "in `makeTime` the given date is added to specified time" ~: do
      let d = date 7 7 2008
      let (Time actual) = makeTime d 14 30
      let (Just expected) =
            parseTimeM True defaultTimeLocale
              "%FT%R" "2008-07-07T14:30" :: Maybe UTCTime
      assertEqual "" expected actual
  ]
