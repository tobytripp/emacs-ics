% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser, many)
import Text.Printf (printf)
import Data.Time.LocalTime (hoursToTimeZone)

import qualified EmacsDiary.Record as R
import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser (diary, record, entry)
\end{code}

\begin{code}
epoch :: R.Record
epoch = onDate 1 1 1970

onDate d m y = R.empty . day $ cdtdate d m y
day     = R.Singular
cdtdate = I.date cdt
\end{code}

Parsers to test:

\begin{code}
diaryParser  = diary cdt
recordParser = record cdt
entryParser  = entry epoch
\end{code}

\begin{code}
tests = test [
  "parse a solitary entry" ~: do
      let input = "  14:30 entry\n"
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["entry"])
  ,

  "parse multi-element entry" ~: do
      let input = unlines [
            "  14:30 field1",
            "    field2",
            "    field3"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2", "field3"])
  ,

  "requires leading whitespace for entry" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "Other Text"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2"])
  ,

  "entry with combinator" ~: do
      let input = unlines [
            "  14:30 field1",
            "  15:30 field2"]
      assertParsesTo (many entryParser) input
        [(R.entry (I.instant I.epoch 19 30) ["field1"]),
         (R.entry (I.instant I.epoch 20 30) ["field2"])]
  ,

  "parse semicolon separator" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "    field3"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2", "field3"])
  ,

  "parse date and time with empty description produces error" ~: do
      let input = "7 July 2008 14:30\n"
      case runParser diaryParser () input input of
        (Left e)  -> assert True
        (Right r) -> assertFailure
          (printf "parsing should have failed, but got '%s'" (show r))
  ,

  "record parsing returns a Record" ~: do
      let input = "7 July 2008\n"
      assertParsesTo diaryParser input [onDate 7 7 2008]
  ,

  "record parser" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      let d = I.date cdt 7 7 2008
      assertParsesTo recordParser input
        (R.push (R.entry (I.instant d 14 30) ["Work on parsers"])
          (R.empty $ R.Singular d))
  ,

  "parse date and time with description" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      let d = I.date cdt 7 7 2008
      let e = R.entry (I.instant d 14 30) ["Work on parsers"]
      assertParsesTo diaryParser input [R.push e (R.empty $ R.Singular d)]
  ,

  "parse multi-line entry" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "    With gusto!",
            ""]
      let d = I.date cdt 7 7 2008
      let e = R.entry (I.instant d 14 30) ["Work on parsers", "With gusto!"]
      case runParser diaryParser () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multi-line entry"
          [R.push e $ onDate 7 7 2008]
          actual
  ,

  -- "weekday entries" ~: do
  --     let input = "Wednesday 08:00 Wake up"
  --     let e = R.entry (I.instant (date 1 1 1970) 8 0) ["Wake up"]
  --     assertParsesTo diaryParser input [R.push e (R.Record (R.Repeating R.Wednesday) [])]
  --     ,

  "parse entry with time-interval" ~: do
      let input = unlines [
            "7 July 2008 13:00-16:00 Gorge on Cake"
            ]
      let d  = I.date cdt 7 7 2008
      let t1 = I.timeOn d 13 0
      let t2 = I.timeOn d 16 0
      let e1 = R.entry (I.interval t1 (Just t2)) ["Gorge on Cake"]
      assertParsesTo diaryParser input
        [R.push e1 (onDate 7 7 2008)]
      ,

  "parse multiple records" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "7 July 2008 15:15 Celebrate"]
      let d = I.date cdt 7 7 2008
      let e1 = R.entry (I.instant d 14 30) ["Work on parsers"]
      let e2 = R.entry (I.instant d 15 15) ["Celebrate"]
      case runParser diaryParser () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multiple records"
          [(R.push e1 $ onDate 7 7 2008),
           (R.push e2 $ onDate 7 7 2008)]
          actual
  ]
\end{code}
