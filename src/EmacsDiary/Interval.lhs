%% -*- coding: utf-8 -*-
\section{Date, Time, and Interval Types}

Note the use of \codeline{(..)} in module imports and exports to ensure that
not only the type, but also its constructor is loaded.

\begin{code}
{-|
Description: Calendar event intervals.
-}
module EmacsDiary.Interval (
  -- * Types
  Date(..),
  WeekDay(..),
  Time(..),
  Interval(..),

  -- ** Exported Types
  -- | Allows consumers of this Module to utilize the functions in
  -- 'Data.Time.LocalTime' including the function 'getCurrentTimeZone', which
  -- is an IO action (i.e., in the IO Monad).
  module Data.Time.LocalTime,
  module Data.Time.Format,

  -- * Convenience constructors
  utcDate,
  makeTime,
  dateOfWeekDay,
  instant,
  interval,
  gregorian,

  -- * Convenience values
  epoch,

  -- * Conversions
  iso8601
  ) where

import Data.Time.Calendar (addDays, fromGregorian, Day)
import Data.Time.LocalTime (zonedTimeToUTC,
  LocalTime(..),
  TimeOfDay(..),
  TimeZone,
  ZonedTime(..),

  localTimeToUTC,
  getCurrentTimeZone,
  utc,
  utcToZonedTime)
import Data.Time.Format (iso8601DateFormat,
  formatTime,
  defaultTimeLocale
  )
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Text.Printf (printf)
import Data.List (elemIndex)
\end{code}

\subsection{Public Interface}

\begin{code}
-- | The 'Date'(s) and 'TimeZone' of a calendar event.
-- All events on a given day are assumed to occur in the same time-zone.
data Date = Date      { calendarDay :: Day, parsedAt :: ZonedTime }
          | DayOfWeek { weekDay :: WeekDay, parsedAt :: ZonedTime }
data Time = Time { date :: Date, time :: UTCTime }

-- | A “range” of times for a calendar event.
data Interval = Interval { start     :: Time,
                           finish    :: Time
                         } deriving (Eq)

-- | Days of the week
data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
  deriving (Eq, Show)

-- | Fort constructing new 'Date' instances.
class MakeDate a where
  utcDate :: ZonedTime -> a -> Date
class Iso8601 a where
  iso8601 :: a -> String

gregorian :: (Integral a, Integral b) =>
              a                 -- ^ Year
            -> b                 -- ^ Month
            -> b                 -- ^ Day
            -> Gregorian

-- | Construct a 'Time’ instance on the given (local) Date, converted to UTC.
makeTime :: Date
         -> Hour
         -> Minute
         -> Time                 -- ^ Time in UTC

dateOfWeekDay :: WeekDay
              -> ZonedTime       -- ^ “current” time
              -> Date            -- ^ the next Day occurring on the given weekday

-- | Construct an 'Interval' that may have zero duration
interval :: Time                 -- ^ start time
         -> Maybe Time           -- ^ end time
         -> Interval             -- ^ 'Interval' in UTC

-- | Construct an 'Interval' from hours:minutes with zero duration
instant :: Date
        -> Hour
        -> Minute
        -> Interval              -- ^ 'Interval' in UTC

-- | The 'Date' of the UNIX Epoch
epoch :: Date
epoch = Date d0 t0
  where
    d0 = fromGregorian 1970 1 1
    t0 = utcToZonedTime utc $ UTCTime d0 0

data Gregorian  = Gregorian {gy :: Year, gm :: Month, gd :: DayOfMonth}
type Hour       = Int
type Minute     = Int
type DayOfMonth = Int
type Month      = Int
type Year       = Integer
\end{code}


\subsection{Implementation}

\subsubsection{Utility Constructors}
\begin{code}
-- | 'MakeDate' instance for the 'Gregorian' type.
instance MakeDate Gregorian where
  utcDate localt (Gregorian y m d) = Date localDay localt
    where
      localDay = (fromGregorian y m d)
      tz       = zonedTimeZone localt
instance MakeDate Day where
  utcDate localt day = Date day localt

gregorian y m d = Gregorian year month day
  where
    year  = toInteger y
    month = fromInteger $ toInteger m
    day   = fromInteger $ toInteger d

makeTime day@(DayOfWeek wd ctime@(ZonedTime _ tz)) h m = Time day utc
  where
    utc   = localTimeToUTC tz local
    local = LocalTime d $ TimeOfDay h m 0
    (Date d _) = dateOfWeekDay wd ctime
makeTime day@(Date d (ZonedTime _ tz)) h m = Time day utc
  where
    utc   = localTimeToUTC tz local
    local = LocalTime d $ TimeOfDay h m 0
\end{code}


Constructors that can handle a missing end-time.

\begin{code}
interval t1 (Just t2) = Interval t1 t2
interval t1 Nothing   = Interval t1 t1

instant d h m =
  interval time Nothing
  where
    time = makeTime d h m
\end{code}

\subsubsection{WeekDays}

Determining the @Date@ of a given @WeekDay@.

\begin{code}
dateOfWeekDay wd current@(ZonedTime (LocalTime d t) tz) =
  utcDate current nextWeekDay
  where
    nextWeekDay         = addDays (toInteger diffDays) d
    (_, currentWeekDay) = sundayStartWeek d
    diffDays            = wdIndex - currentWeekDay
    (Just wdIndex)      = elemIndex wd weekdays
    weekdays = [
      Sunday,
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday
      ]
\end{code}


\subsubsection{Show Instances}

The \codeline{Show} instance for \codeline{Interval} demonstrates Haskell
“guards” in function definitions.

\begin{code}
instance Show Interval where
  show (Interval a b)
    | a == b     = showTZ a
    | otherwise = (showT a) ++ "-" ++ (showTZ b)

showT  (Time _ t) = formatTime defaultTimeLocale "%Y%m%dT%H:%M" t
showTZ (Time _ t) = formatTime defaultTimeLocale "%Y%m%dT%H:%M %Z" t
\end{code}

\begin{code}
instance Show Date where
  show (Date d tz) = formatTime defaultTimeLocale format d
    where
      format = iso8601DateFormat (Just "%H%M%SZ")
  show (DayOfWeek wd tz) = printf "%s (%s)" (show wd) (show tz)
instance Iso8601 Date where
  iso8601 d = show d

instance Show Time where
  show (Time d t) = formatTime defaultTimeLocale format t
    where
      format = iso8601DateFormat (Just "%H%M%SZ")
instance Iso8601 Time where
  iso8601 t = show t

instance Iso8601 ZonedTime where
  iso8601 zt@(ZonedTime t tz) = formatTime defaultTimeLocale format utc
    where
      format = iso8601DateFormat (Just "%H%M%SZ")
      utc = zonedTimeToUTC zt

instance Eq Time where
  (Time _ t1) == (Time _ t2) = t1 == t2
instance Eq Date where
  (Date d1 _) == (Date d2 _) = d1 == d2
  (DayOfWeek d1 _) == (DayOfWeek d2 _) = d1 == d2
\end{code}
