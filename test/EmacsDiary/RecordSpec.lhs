% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (recordTests) where

import Test.HUnit
import SpecHelpers

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import qualified EmacsDiary.Ics as ICS
\end{code}

\begin{code}
recordTests = test [
  "create a DiaryEntry" ~: do
    let (Right d) = parseTimeF "%e %b %Y" "7 July 2008"
    let t = I.mkInterval d Nothing
    " 7 July 2008 Chicago, IL" @=? show (Entry t [Location "Chicago, IL"])
  ,

  "emit a diary record as ics" ~: do
    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "DTSTART:20180707T120000Z"
          , "DTEND:20180707T120000Z"
          , "SUMMARY:Happy Birthday, Son!"
          , "END:VEVENT"
          , "BEGIN:VEVENT"
          , "DTSTART:20180707T130000Z"
          , "DTEND:20180707T140000Z"
          , "SUMMARY:Eat Cake."
          , "END:VEVENT"
          , "END:VCALENDAR"
          ]
    let e1 = Entry (I.makeInterval 12 00) [Description "Happy Birthday, Son!"]
    let e2 = Entry (I.mkInterval (I.makeTime 13 00) (Just (I.makeTime 14 00)))
              [Description "Eat Cake."]
    let input = Diary [
          push e1 $ push e2 (empty (I.fromDmy 7 7 2008))
          ]

    assertEqual "" expected (ICS.toIcs input)
  ]
\end{code}
