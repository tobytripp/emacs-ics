% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (tests) where

import Test.HUnit
import SpecHelpers

import Data.Time.Calendar
import Data.Time.LocalTime

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import qualified EmacsDiary.Ics as ICS
\end{code}

\begin{code}
tstamp = ZonedTime local cdt
  where
    local = LocalTime d t
    d = fromGregorian 2008 07 07
    t = TimeOfDay 12 00 00
\end{code}


\begin{code}
tests = test [
\end{code}

\begin{code}
  "extract meta-data from an entry" ~: do
      let d = I.utcDate tstamp (I.gregorian 2008 7 7)
      let e = Entry (I.instant d 14 30) [
            Description "Coffee-Thirty"
            , Location "ThoughtWorks, 200 E Randolph, Chicago, IL"
            , Description "Sacrosanct Event"]

      let expected
            = MetaData
                (Description "Coffee-Thirty")
                [Description "Sacrosanct Event"]
                [Location "ThoughtWorks, 200 E Randolph, Chicago, IL"]

      let actual = metadata e

      assertEqual "" expected actual
  ,
\end{code}

\begin{code}
  "emit a diary record as ics" ~: do
    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "UID:ED20080707170000"
          , "CREATED:20080707T170000Z"
          , "DTSTART:20080707T170000Z"
          , "DTEND:20080707T170000Z"
          , "SUMMARY:Happy Birthday, Son!"
          , "END:VEVENT"
          , "BEGIN:VEVENT"
          , "UID:ED20080707180000"
          , "CREATED:20080707T170000Z"
          , "DTSTART:20080707T180000Z"
          , "DTEND:20080707T190000Z"
          , "SUMMARY:Eat Cake."
          , "END:VEVENT"
          , ""
          , "END:VCALENDAR"
          ]
    let  d = I.utcDate tstamp (I.gregorian 2008 7 7)
    let e1 = Entry (I.instant d 12 00) [Description "Happy Birthday, Son!"]
    let e2 = Entry (I.interval (I.makeTime d 13 00)
                    (Just (I.makeTime d 14 00)))
              [Description "Eat Cake."]
    let input = Diary [
          push e1 $ push e2 (empty d)
          ]

    assertEqual "" expected (ICS.toIcs input)
  ,
\end{code}


\begin{code}
  "repeating events as ICS" ~: do
    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "UID:ED20080709193000"
          , "CREATED:20080707T170000Z"
          , "DTSTART:20080709T193000Z"
          , "DTEND:20080709T193000Z"
          , "RRULE:FREQ=WEEKLY"
          , "SUMMARY:Coffee"
          , "END:VEVENT"
          , ""
          , "END:VCALENDAR"
          ]
    let d = I.DayOfWeek I.Wednesday tstamp
    let e = Entry (I.instant d 14 30) [Description "Coffee"]
    let input = Diary [push e (empty d)]
    assertEqual "" expected (ICS.toIcs input)
  ,
\end{code}


At least for MacOS’s Calendar application, having multiple description fields
causes the import to silently stop.  That is, further entries are not imported
cand no error is displayed.

\begin{code}
  "multiple descriptions will not load" ~: do
    let d = I.utcDate tstamp (I.gregorian 2008 7 7)
    let e = Entry (I.instant d 14 30) [
          Description "Coffee"
          , Description "Coffee-Thirty"
          , Description "Sacrosanct Event"]
    let input = Diary [push e (empty d)]

    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "UID:ED20080707193000"
          , "CREATED:20080707T170000Z"
          , "DTSTART:20080707T193000Z"
          , "DTEND:20080707T193000Z"
          , "SUMMARY:Coffee"
          , "DESCRIPTION:Coffee-Thirty;Sacrosanct Event"
          , "END:VEVENT"
          , ""
          , "END:VCALENDAR"
          ]
    assertEqual "" expected (ICS.toIcs input)
  ,
\end{code}

\begin{code}
  "render location attribute with vcal location field" ~: do
    let d = I.utcDate tstamp (I.gregorian 2008 7 7)
    let e = Entry (I.instant d 14 30) [
          Description "Coffee-Thirty"
          , Location "ThoughtWorks, 200 E Randolph, Chicago, IL"
          , Description "Sacrosanct Event"]
    let input = Diary [push e (empty d)]

    let expected = unlines [
          "BEGIN:VCALENDAR"
          , "VERSION:2.0"
          , "BEGIN:VEVENT"
          , "UID:ED20080707193000"
          , "CREATED:20080707T170000Z"
          , "DTSTART:20080707T193000Z"
          , "DTEND:20080707T193000Z"
          , "SUMMARY:Coffee-Thirty"
          , "DESCRIPTION:Sacrosanct Event"
          , "LOCATION:ThoughtWorks\\, 200 E Randolph\\, Chicago\\, IL"
          , "END:VEVENT"
          , ""
          , "END:VCALENDAR"
          ]
    assertEqual "" expected (ICS.toIcs input)
\end{code}

\begin{code}
  ]
\end{code}
