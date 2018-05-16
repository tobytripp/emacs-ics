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
import qualified EmacsDiary.Interval as I

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
\end{code}

\begin{code}
date     :: I.TimeZone -> Parser I.Date
time     :: I.Date -> Parser I.Time
interval :: I.Date -> Parser I.Interval
\end{code}

\subsection{Date}

\begin{code}
date tz = I.date tz <$> dayP <*> monthP <*> yearP <?> "date"

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

-- | Map month-names to integer values
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
  return $ I.timeOn d (fromInteger h) (fromInteger m)
\end{code}

\subsection{Interval}

An \codeline{Interval} is a range of times on a specified \codeline{Date}.
The Emacs Diary, so far as I know, does not support events spanning more than
one day.

\begin{code}
interval d = do
  t1 <- try (time d) <|> unexpected "start time"
  t2 <- option t1 (T.symbol "-" *> (time d))
  return $ I.Interval t1 t2
\end{code}
