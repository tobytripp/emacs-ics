%% -*- coding: utf-8 -*-
\section{Date and Time Types}

\begin{code}
module EmacsDiary.Interval (Date(..), fromYmd, Time(..), timeFromList) where

import Data.Fixed
import Data.Time.Calendar (fromGregorian, showGregorian, Day)
import Data.Time.Format (
  FormatTime(..),
  iso8601DateFormat,
  formatTime,
  parseTimeM,
  defaultTimeLocale
  )
\end{code}

Note the use of \codeline{(..)} to ensure that not only the type, but also its
constructor is loaded.

\begin{code}
import Data.Time.Clock (
  diffTimeToPicoseconds,
  secondsToDiffTime,
  addUTCTime,
  DiffTime,
  NominalDiffTime,
  utctDayTime,
  UTCTime(..))
\end{code}

\subsection{Date}

\blockquote[{\cite[/NewType]{hswiki}}]{%
A newtype declaration creates a new type in much the same way as data. The
syntax and usage of newtypes is virtually identical to that of data
declarations - in fact, you can replace the newtype keyword with data and
it'll still compile, indeed there's even a good chance your program will still
work. The converse is not true, however - data can only be replaced with
newtype if the type has exactly one constructor with exactly one field inside
it. }

\begin{code}
newtype Date = Date Day deriving (Eq)
\end{code}

I’ll \codeline{show} \codeline{Date} using the ISO-8601 format:

\begin{code}
instance Show Date where
  show (Date d) = showGregorian d

fromYmd :: Integer -> Int -> Int -> Date
fromYmd y m d = Date $ fromGregorian y m d
\end{code}

\codeline{Time} will represent instances of UTC times.

\begin{code}
type Time = UTCTime

timeFromList :: [Integer] -> Time
timeFromList (h:m:_) =
  fromInt $ h * 3600 + m * 60
  where
    fromInt i = UTCTime epoch (secondsToDiffTime . fromInteger $ i)
    epoch = fromGregorian 1970 1 1

newtype Seconds = Seconds Integer deriving (Eq, Show)
instance Num Seconds where
  (Seconds a) + (Seconds b) = Seconds (a + b)
  (Seconds a) - (Seconds b) = Seconds (a - b)
  (Seconds a) * (Seconds b) = Seconds (a * b)
  negate (Seconds i) = Seconds (negate i)
  abs (Seconds i)    = Seconds (abs i)
  signum (Seconds i) = Seconds (signum i)
  fromInteger i      = Seconds (fromInteger i)

newtype PicoSeconds = PicoSeconds Integer deriving (Eq, Show)
instance Num PicoSeconds where
  (PicoSeconds a) + (PicoSeconds b) = PicoSeconds (a + b)
  (PicoSeconds a) - (PicoSeconds b) = PicoSeconds (a - b)
  (PicoSeconds a) * (PicoSeconds b) = PicoSeconds (a * b)
  negate (PicoSeconds i) = PicoSeconds (negate i)
  abs (PicoSeconds i)    = PicoSeconds (abs i)
  signum (PicoSeconds i) = PicoSeconds (signum i)
  fromInteger i          = PicoSeconds (fromInteger i)

fromPicoSeconds :: PicoSeconds -> Seconds
fromPicoSeconds (PicoSeconds p) = Seconds $ p * 10^12

picoSeconds :: UTCTime -> PicoSeconds
picoSeconds = PicoSeconds . diffTimeToPicoseconds . utctDayTime

seconds :: Time -> Seconds
seconds = fromPicoSeconds . picoSeconds

timeToNDiff :: Time -> NominalDiffTime
timeToNDiff = fromInteger . diffTimeToPicoseconds . utctDayTime

addTime :: UTCTime -> Time -> UTCTime
addTime a t = addUTCTime (timeToNDiff t) a
\end{code}
