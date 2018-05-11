% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (tests) where

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
          , "DTSTART:20080707T120000Z"
          , "DTEND:20080707T120000Z"
          , "SUMMARY:Happy Birthday, Son!"
          , "END:VEVENT"
          , ""
          , "BEGIN:VEVENT"
          , "DTSTART:20080707T130000Z"
          , "DTEND:20080707T140000Z"
          , "SUMMARY:Eat Cake."
          , "END:VEVENT"
          , ""
          , ""
          , "END:VCALENDAR"
          ]
    let  d = I.date 7 7 2008
    let e1 = Entry (I.instant d 12 00) [Description "Happy Birthday, Son!"]
    let e2 = Entry (I.mkInterval (I.makeTime d 13 00)
                    (Just (I.makeTime d 14 00)))
              [Description "Eat Cake."]
    let input = Diary [
          push e1 $ push e2 (empty (Singular d))
          ]

    assertEqual "" expected (ICS.toIcs input)
  ]
\end{code}
