%% -*- coding: utf-8 -*-
\section{Parsing Dates and Times}

\begin{code}
{-|
Description: Parsers for calendar time-intervals.
-}
module EmacsDiary.Parser.Interval (
  interval,
  date,
  time
  )where

import qualified EmacsDiary.Parser.Tokens as T
import EmacsDiary.Interval (
  Date(..),
  WeekDay(..),
  Time,
  Interval(..),

  utcDate,
  makeTime,
  gregorian
  )

import Text.Parsec (
  (<|>),                        -- ^ 'choice' operator
  (<?>),                        -- ^ 'unexpected' operator

  choice,
  option,
  string,
  try,
  unexpected
  )
import Text.Parsec.String (Parser)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (
  ZonedTime,
  zonedTimeZone
  )
\end{code}

\begin{code}
date     :: ZonedTime            -- ^ current local time
         -> Parser Date
time     :: Date -> Parser Time
interval :: Date -> Parser Interval
\end{code}

\subsection{Date}

\begin{code}
date localtime = day localtime <|> weekday localtime

day localt = do
  d <- dayP
  m <- monthP
  y <- yearP <?> "date"
  return $ utcDate localt (gregorian y m d)
weekday localt = T.lexeme $ choice $
  map kvp weekdays
  where
    kvp :: (String, WeekDay) -> Parser Date
    kvp (wstring, wd) = try (string wstring) >> return (DayOfWeek wd localt)
    weekdays = [
        ("Sunday",    Sunday)
      , ("Monday",    Monday)
      , ("Tuesday",   Tuesday)
      , ("Wednesday", Wednesday)
      , ("Thursday",  Thursday)
      , ("Friday",    Friday)
      , ("Saturday",  Saturday)
      ]

dayP :: Parser Int
dayP  = fromIntegral <$> T.numeric

yearP :: Parser Integer
yearP = T.numeric

monthP :: Parser Int
monthP = T.lexeme $ choice $
  map kvp months
  where
    kvp :: (String, Int) -> Parser Int
    kvp (mn,n) = try (string mn) >> return n

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
\end{code}

\subsection{Time}

\begin{code}
time d = do
  h <- T.whitespace *> try (T.numeric <* T.symbol ":") <|> unexpected "time"
  m <- T.numeric <|> unexpected "time"
  return $ makeTime d (fromInteger h) (fromInteger m)
\end{code}

\subsection{Interval}

An \codeline{Interval} is a range of times on a specified \codeline{Date}.
The Emacs Diary, so far as I know, does not support events spanning more than
one day.

\begin{code}
interval d = do
  t1 <- try (time d) <|> unexpected "start time"
  t2 <- option t1 (T.symbol "-" *> (time d))
  return $ Interval t1 t2
\end{code}
