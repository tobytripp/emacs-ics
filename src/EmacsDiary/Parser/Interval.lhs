%% -*- coding: utf-8 -*-
\section{Parsing Dates and Times}

\begin{code}
module EmacsDiary.Parser.Interval where

import qualified EmacsDiary.Parser.Tokens as T

import Text.Parsec (
  many, string, count, spaces, digit, many1, choice, (<|>), try, sepBy1, unexpected)
import Text.Parsec.String (Parser)
import Data.Time.Clock (
  addUTCTime,
  NominalDiffTime,
  utctDayTime,
  utctDay,
  UTCTime)
import Data.Time.Calendar (fromGregorian, showGregorian, Day)
import Data.Time.Format (iso8601DateFormat, formatTime, parseTimeM, defaultTimeLocale)
import Text.Printf (printf)

import Control.Monad (void)
\end{code}

\begin{code}
showTimeFormat = "%H:%M:%S%z"
locale = defaultTimeLocale
\end{code}

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

\codeline{Time} will represent instances of UTC times.
\begin{code}
data Time = Time { timeHour   :: Integer
                 , timeMinute :: Integer
                 } deriving (Eq)
instance Show Time where
         show t = iso8601DateFormat (Just showTimeFormat)

instance Num Time where
  a + b  = fromInteger $ (seconds a) + (seconds b)
  a * b  = fromInteger $ (seconds a) * (seconds b)
  abs    = fromInteger . abs . seconds
  signum = fromInteger . signum . seconds
  negate = fromInteger . negate . seconds
  fromInteger a = Time h m
    where
      i = fromInteger a
      h = quot i 3600
      m = div s 60
      s = rem i 3600

timeFromList :: [Integer] -> Time
timeFromList (h:m:_) = Time h m

seconds :: Time -> Integer
seconds (Time h m)= 3600*h + 60*m

timeToNDiff :: Time -> NominalDiffTime
timeToNDiff (Time h m) = fromInteger (3600*h + 60*m)

addTime :: UTCTime -> Time -> UTCTime
addTime a t = addUTCTime (timeToNDiff t) a
\end{code}

\subsection{Parsers}

\begin{code}
decided :: Parser (Either String a) -> Parser a
decided p = do
        e <- p
        case e of
          (Left s)  -> unexpected s
          (Right r) -> return r

timeS :: Parser Time
timeS = timeFromList <$> sepBy1 T.numeric (T.symbol ":")

dateFormats = [
  "%d %b %Y",
  "%d %B %Y",
  "%e %b %Y",
  "%e %B %Y",
  "%F"]

--date :: Parser UTCTime
--date = decided p
--  where
--    p = asUTC <$> dateS

-- dateString :: Parser String
-- dateString = do
--   void spaces
--   d <- digits
--   m <- word
--   y <- digits
--   return (d:(m:y))
--     -- $ parseDateS (printf "%s %s %s" d m y)

dayP :: Parser Int
dayP  = fromIntegral <$> T.numeric

yearP :: Parser Integer
yearP = T.numeric

months = [
  ("Jan",       1),  ("January",   1),
  ("Feb",       2),  ("February",  2),
  ("Mar",       3),  ("March",     3),
  ("Apr",       4),  ("April",     4),
  ("May",       5),
  ("Jun",       6),  ("June",      6),
  ("July",       7),  ("Jul",      7),
  ("Aug",       8),  ("August",    8),
  ("Sep",       9),  ("September", 9),
  ("Oct",       10), ("October",   10),
  ("Nov",       11), ("November",  11),
  ("Dec",       12), ("December",  12)
  ]

keyValueParser :: (String, Int) -> Parser Int
keyValueParser (m,n) = try (string m) >> return n

monthP :: Parser Int
monthP = choice $ map keyValueParser months

-- date :: Parser Date
date :: Parser Day
date = do
  d <- dayP
  m <- T.lexeme monthP
  y <- yearP
  return $ fromGregorian y m d

time :: UTCTime -> Parser UTCTime
time datetime = addTime datetime <$> timeS
\end{code}
