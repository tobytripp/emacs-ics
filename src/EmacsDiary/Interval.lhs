%% -*- coding: utf-8 -*-
\section{Date and Time Types}

\begin{code}
module EmacsDiary.Interval (
  Date(..)
  , date
  , epoch
  , Time(..)
  , makeTime
  , instant
  , Interval(..)
  , mkInterval
  ) where

import Data.Fixed
import Data.Time.Calendar (fromGregorian, showGregorian, Day)
import Data.Time.Format (
  formatTime,
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

\blockquote[{\autocite[/NewType]{haskellwiki}}]{%
A newtype declaration creates a new type in much the same way as data. The
syntax and usage of newtypes is virtually identical to that of data
declarations - in fact, you can replace the newtype keyword with data and
it'll still compile, indeed there's even a good chance your program will still
work. The converse is not true, however - data can only be replaced with
newtype if the type has exactly one constructor with exactly one field inside
it. }

\begin{code}
newtype Date = Date Day deriving (Eq)
newtype Time = Time UTCTime deriving (Eq)
epoch = Date $ fromGregorian 1970 1 1
\end{code}

\codeline{Interval} will represent a “range” of \codeline{Time}s.

\begin{code}
data Interval = Interval { start  :: Time,
                           finish :: Time
                         } deriving (Eq)
\end{code}

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
  show (Date d) = formatTime defaultTimeLocale "%Y%m%d" d
instance Show Time where
  show (Time t)= formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" t

date :: Int -> Int -> Integer -> Date
date d m y = Date $  fromGregorian y m d
\end{code}

\begin{code}
makeTime :: Date -> Integer -> Integer -> Time
makeTime (Date d) h m =
  Time . fromInt $ secs
  where
    secs      = h * 3600 + m * 60
    fromInt i = UTCTime d (secondsToDiffTime . fromInteger $ i)


timeFromList :: [Integer] -> Time
timeFromList (h:xs) =
  makeTime epoch h m
  where
    m = case take 1 xs of
      [] -> 0
      (m:_) -> m

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
  abs    (PicoSeconds i) = PicoSeconds (abs i)
  signum (PicoSeconds i) = PicoSeconds (signum i)
  fromInteger i          = PicoSeconds (fromInteger i)

fromPicoSeconds :: PicoSeconds -> Seconds
fromPicoSeconds (PicoSeconds p) = Seconds $ p * 10^12

picoSeconds :: Time -> PicoSeconds
picoSeconds (Time t) = PicoSeconds . diffTimeToPicoseconds . utctDayTime $ t

seconds :: Time -> Seconds
seconds = fromPicoSeconds . picoSeconds

timeToNDiff :: Time -> NominalDiffTime
timeToNDiff (Time t)= fromInteger . diffTimeToPicoseconds . utctDayTime $ t

addTime :: UTCTime -> Time -> UTCTime
addTime a t = addUTCTime (timeToNDiff t) a
\end{code}



Constructors that can handle a missing end-time.

\begin{code}
mkInterval :: Time -> Maybe Time -> Interval
mkInterval a (Just b) = Interval a b
mkInterval a Nothing  = Interval a a

mkTime :: Time -> Interval
mkTime t = mkInterval t Nothing

instant :: Date -> Integer -> Integer -> Interval
instant d h m = mkTime $ makeTime d h m
\end{code}
