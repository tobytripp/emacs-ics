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

Create a “parsed-at” time-stamp value for use in testing parsers.

\begin{code}
import Data.Time.Calendar
import Data.Time.LocalTime

tstamp = ZonedTime local cdt
  where
    local = LocalTime d t
    d = fromGregorian 2008 07 07
    t = TimeOfDay 12 0 0
\end{code}

A sample @Record@

\begin{code}
epochRecord = makeRecord (I.gregorian 1970 1 1)
makeRecord  = R.empty . (I.utcDate tstamp)
\end{code}

Parsers to test:

\begin{code}
diaryParser  = diary  tstamp
recordParser = record tstamp
entryParser  = entry  epochRecord
\end{code}

\begin{code}
tests = test [
  "parse a solitary entry" ~: do
      let input = "  14:30 entry\n"
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["entry"])
  ,
\end{code}


\begin{code}
  "parse multi-element entry" ~: do
      let input = unlines [
            "  14:30 field1",
            "    field2",
            "    field3"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2", "field3"])
  ,

\end{code}


\begin{code}
  "requires leading whitespace for entry" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "Other Text"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2"])
  ,

\end{code}


\begin{code}
  "entry with combinator" ~: do
      let input = unlines [
            "  14:30 field1",
            "  15:30 field2"]
      assertParsesTo (many entryParser) input
        [(R.entry (I.instant I.epoch 19 30) ["field1"]),
         (R.entry (I.instant I.epoch 20 30) ["field2"])]
  ,

\end{code}


\begin{code}
  "parse semicolon separator" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "    field3"
            ]
      assertParsesTo entryParser input
        (R.entry (I.instant I.epoch 19 30) ["field1", "field2", "field3"])
  ,

\end{code}


\begin{code}
  "parse date and time with empty description produces error" ~: do
      let input = "7 July 2008 14:30\n"
      case runParser diaryParser () input input of
        (Left e)  -> assert True
        (Right r) -> assertFailure
          (printf "parsing should have failed, but got '%s'" (show r))
  ,

\end{code}


\begin{code}
  "record parsing returns a Record" ~: do
      let input = "7 July 2008\n"
      assertParsesTo diaryParser input (R.Diary [makeRecord (I.gregorian 2008 7 7)])
  ,

\end{code}


\begin{code}
  "record parser" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      let d = I.utcDate tstamp (I.gregorian 2008 7 7)
      assertParsesTo recordParser input
        (R.push (R.entry (I.instant d 14 30) ["Work on parsers"])
          (R.empty d))
  ,

\end{code}


\begin{code}
  "parse date and time with description" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      let d = I.utcDate tstamp (I.gregorian 2008 7 7)
      let e = R.entry (I.instant d 14 30) ["Work on parsers"]
      assertParsesTo diaryParser input (R.Diary [R.push e (R.empty d)])
  ,

\end{code}


\begin{code}
  "parse multi-line entry" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "    With gusto!",
            ""]
      let d = I.utcDate tstamp (I.gregorian 2008 7 7)
      let e = R.entry (I.instant d 14 30) ["Work on parsers", "With gusto!"]
      case runParser diaryParser () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multi-line entry"
          (R.Diary [R.push e $ makeRecord (I.gregorian 2008 7 7)])
          actual
  ,
\end{code}

In the Emacs Diary, weekly repeating events can be specified by using a
week-day name instead of a specific date.

\begin{code}
  "weekday entries" ~: do
      let input = "Wednesday 08:00 Wake up"
      let e = R.entry (I.instant (I.utcDate tstamp (I.gregorian 2008 07 09)) 8 0) ["Wake up"]
      assertParsesTo diaryParser input (R.Diary [
        R.push e (R.Record (I.DayOfWeek I.Wednesday tstamp) [])])
      ,
\end{code}

\begin{code}
  "parse entry with time-interval" ~: do
      let input = unlines [
            "7 July 2008 13:00-16:00 Gorge on Cake"
            ]
      let d  = I.utcDate tstamp (I.gregorian 2008 7 7)
      let t1 = I.makeTime d 13 0
      let t2 = I.makeTime d 16 0
      let e1 = R.entry (I.interval t1 (Just t2)) ["Gorge on Cake"]
      assertParsesTo diaryParser input
        (R.Diary [R.push e1 (makeRecord (I.gregorian 2008 7 7))])
      ,
\end{code}


\begin{code}
  "parse multiple records" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "7 July 2008 15:15 Celebrate"]
      let d = I.utcDate tstamp (I.gregorian 2008 7 7)
      let e1 = R.entry (I.instant d 14 30) ["Work on parsers"]
      let e2 = R.entry (I.instant d 15 15) ["Celebrate"]
      case runParser diaryParser () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multiple records"
          (R.Diary [(R.push e1 $ makeRecord (I.gregorian 2008 7 7)),
           (R.push e2 $ makeRecord (I.gregorian 2008 7 7))])
          actual
  ]
\end{code}
