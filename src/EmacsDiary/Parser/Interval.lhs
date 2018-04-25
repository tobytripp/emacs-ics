%% -*- coding: utf-8 -*-
\section{Parsing Dates and Times}

\begin{code}
module EmacsDiary.Parser.Interval where

import qualified EmacsDiary.Parser.Tokens as T

import Control.Monad (void)
import Text.Parsec (
  many, string, count, spaces, digit, many1, choice, (<|>), try, sepBy1, unexpected)
import Text.Parsec.String (Parser)
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
\subsection{Type Definitions}

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
\end{code}

\subsubsection{Parsers}

\begin{code}
dayP :: Parser Int
dayP  = fromIntegral <$> T.numeric

yearP :: Parser Integer
yearP = T.numeric

months = [
  ("January",   1),  ("Jan",       1),
  ("February",  2),  ("Feb",       2),
  ("March",     3),  ("Mar",       3),
  ("April",     4),  ("Apr",       4),
  ("May",       5),
  ("June",      6),  ("Jun",       6),
  ("July",      7),  ("Jul",       7),
  ("August",    8),  ("Aug",       8),
  ("September", 9),  ("Sep",       9),
  ("October",   10), ("Oct",       10),
  ("November",  11), ("Nov",       11),
  ("December",  12), ("Dec",       12)
  ]

keyValueParser :: (String, Int) -> Parser Int
keyValueParser (m,n) = try (string m) >> return n

monthP :: Parser Int
monthP = choice $ map keyValueParser months

date :: Parser Date
date = do
  d <- dayP
  m <- T.lexeme monthP
  y <- yearP
  return $ Date $ fromGregorian y m d
\end{code}

\subsection{Time}

\subsubsection{Types}
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

\subsubsection{Parsers}

\begin{code}
time :: Parser Time
time = timeFromList <$> sepBy1 T.numeric (T.symbol ":")
\end{code}
