% -*- coding: utf-8 -*-
\begin{code}
module EmacsDiary.IntervalSpec (tests) where
\end{code}

\begin{code}
import Test.HUnit
import SpecHelpers

import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime

import EmacsDiary.Interval
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
  "in `makeTime` the given date is added to specified date" ~: do
      let d = utcDate tstamp (gregorian 2008 7 7)
      let (Time _ actual) = makeTime d 14 30
      let (Just expected) =
            parseTimeM True defaultTimeLocale
              "%FT%R" "2008-07-07T19:30" :: Maybe UTCTime
      assertEqual "" expected actual
  ,
\end{code}

\begin{code}
  "local time-zone is converted to UTC" ~: do
      let d@(Date day _)  = utcDate tstamp (gregorian 2008 7 7)
      let expected        = UTCTime day 70200
      let (Time _ actual) = makeTime d 14 30

      assertEqual "" expected actual
  ,
\end{code}

\begin{code}
  "local time formats as UTC" ~: do
      let d@(Date day _)  = utcDate tstamp (gregorian 2008 7 7)
      let expected        = UTCTime day 70200
      let (Time _ actual) = makeTime d 14 30

      assertEqual ""
        (formatTime defaultTimeLocale "%FT%R%Z" expected)
        (formatTime defaultTimeLocale "%FT%R%Z" actual)
  ,
\end{code}

\begin{code}
  "weekdays use the current date to find the next weekday's date" ~: do
      let actual   = dateOfWeekDay Tuesday tstamp
      let expected = utcDate tstamp (gregorian 2008 7 8)
      assertEqual "" expected actual
  ]
\end{code}
