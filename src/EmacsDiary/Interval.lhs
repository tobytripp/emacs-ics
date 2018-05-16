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

  -- * Convenience constructors
  fromNumbers,
  timeOn,
  instant,
  interval,

  -- * Convenience values
  epoch
  ) where

import Data.Time.Calendar (fromGregorian, Day)
import Data.Time.LocalTime (
  LocalTime(..),
  TimeOfDay(..),
  TimeZone,
  localTimeToUTC,
  getCurrentTimeZone,
  utc)
import Data.Time.Format (
  formatTime,
  defaultTimeLocale
  )
import Data.Time.Clock (UTCTime(..))
import Text.Printf (printf)
\end{code}

\subsection{Public Interface}

\blockquote[{\autocite[/NewType]{haskellwiki}}]{%
A newtype declaration creates a new type in much the same way as data. The
syntax and usage of newtypes is virtually identical to that of data
declarations - in fact, you can replace the newtype keyword with data and
it'll still compile, indeed there's even a good chance your program will still
work. The converse is not true, however - data can only be replaced with
newtype if the type has exactly one constructor with exactly one field inside
it. }

\begin{code}
newtype Time = Time UTCTime deriving (Eq)

-- | The 'Date'(s) and 'TimeZone' of a calendar event.
-- All events on a given day are assumed to occur in the same time-zone.
data Date = Date { calendarDay :: Day, tz :: TimeZone }
          | DayOfWeek { weekDay :: WeekDay, tz :: TimeZone }
  deriving (Eq)

-- | A “range” of times for a calendar event.
data Interval = Interval { start  :: Time,
                           finish :: Time
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

-- | Construct a new 'Date' instance.
fromNumbers :: TimeZone                 -- ^ time-zone
     -> Int                      -- ^ day
     -> Int                      -- ^ month
     -> Integer                  -- ^ year
     -> Date

-- | Construct a 'Time’ instance on the given Date, in UTC.
-- TODO: Consider using Hours/Minutes as explicit types here.
timeOn :: Date
       -> Int                    -- ^ hours
       -> Int                    -- ^ minutes
       -> Time                   -- ^ Time in UTC

-- | Construct an 'Interval' that may have zero duration
interval :: Time                 -- ^ start time
         -> Maybe Time           -- ^ end time
         -> Interval

-- | Construct an 'Interval' from hours:minutes with no end-time
instant :: Date
        -> Int                   -- ^ hours
        -> Int                   -- ^ minutes
        -> Interval              -- ^ 'Interval' in UTC

-- | The 'Date' of the UNIX Epoch
epoch :: Date
epoch = Date t0 utc
  where
    t0 = fromGregorian 1970 1 1
\end{code}

\subsection{Implementation}

\subsubsection{Utility Constructors}
\begin{code}
fromNumbers tz d m y = Date (fromGregorian y m d) tz

timeOn (DayOfWeek _ tz) h m = Time utc
  where
    utc   = localTimeToUTC tz local
    local = LocalTime epoch $ TimeOfDay h m 0
    epoch = fromGregorian 1970 1 1
timeOn (Date d tz) h m = Time utc
  where
    utc   = localTimeToUTC tz local
    local = LocalTime d $ TimeOfDay h m 0
\end{code}

Constructors that can handle a missing end-time.

\begin{code}
interval a (Just b) = Interval a b
interval a Nothing  = Interval a a

instant d h m =
  mkInterval time
  where
    mkInterval t = interval t Nothing
    time = timeOn d h m
\end{code}

\subsubsection{Show Instances}

The \codeline{Show} instance for \codeline{Interval} demonstrates Haskell
“guards” in function definitions.

\begin{code}
instance Show Interval where
  show (Interval a b)
    | a == b     = showTZ a
    | otherwise = (showT a) ++ "-" ++ (showTZ b)

showT  (Time t) = formatTime defaultTimeLocale "%Y%m%dT%H:%M" t
showTZ (Time t) = formatTime defaultTimeLocale "%Y%m%dT%H:%M %Z" t
\end{code}

\begin{code}
instance Show Date where
  show (Date d tz) = formatTime defaultTimeLocale format d
    where
      format = printf "%%Y%%m%%d (%s)" (show tz)
  show (DayOfWeek wd tz) = printf "%s (%s)" (show wd) (show tz)
instance Show Time where
  show (Time t)= formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" t
\end{code}
