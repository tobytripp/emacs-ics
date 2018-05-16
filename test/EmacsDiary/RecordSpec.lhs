% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (tests) where

import Data.Time.LocalTime (hoursToTimeZone)

import Test.HUnit
import SpecHelpers

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import qualified EmacsDiary.Ics as ICS
\end{code}

\begin{code}
tests = test [
  "emit a diary record as ics" ~: do
    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "DTSTART:20080707T170000Z"
          , "DTEND:20080707T170000Z"
          , "SUMMARY:Happy Birthday, Son!"
          , "END:VEVENT"
          , ""
          , "BEGIN:VEVENT"
          , "DTSTART:20080707T180000Z"
          , "DTEND:20080707T190000Z"
          , "SUMMARY:Eat Cake."
          , "END:VEVENT"
          , ""
          , ""
          , "END:VCALENDAR"
          ]
    let  d = I.fromNumbers cdt 7 7 2008
    let e1 = Entry (I.instant d 12 00) [Description "Happy Birthday, Son!"]
    let e2 = Entry (I.interval (I.timeOn d 13 00)
                    (Just (I.timeOn d 14 00)))
              [Description "Eat Cake."]
    let input = Diary [
          push e1 $ push e2 (empty d)
          ]

    assertEqual "" expected (ICS.toIcs input)
  ]
\end{code}
