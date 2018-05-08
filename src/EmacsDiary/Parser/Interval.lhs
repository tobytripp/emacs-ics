%% -*- coding: utf-8 -*-
\section{Parsing Dates and Times}

\begin{code}
module EmacsDiary.Parser.Interval where

import qualified EmacsDiary.Parser.Tokens as T
import EmacsDiary.Interval

import Data.Functor.Identity

import Text.Parsec (Column, Parsec, ParsecT, Stream, (<?>),
  many, string, count, spaces, digit, many1, choice, (<|>), try, sepBy1, unexpected)
import Text.Parsec.String (Parser)
import Data.Time.Calendar (fromGregorian)
\end{code}

\subsection{Date}

\begin{code}
--dayP :: Parser Int
dayP  = fromIntegral <$> T.numeric

--yearP :: Parser Integer
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

monthP :: Parser Int
monthP = T.lexeme $ choice $
  map kvp months
  where
    kvp :: (String, Int) -> Parser Int
    kvp (mn,n) = try (string mn) >> return n

date :: Parser Date
date = fromDmy <$> dayP <*> monthP <*> yearP <?> "date"
\end{code}

\subsection{Time}

\begin{code}
time :: Parser Time
time = do
  h <- T.whitespace *> try (T.numeric <* T.symbol ":") <|> unexpected "time"
  m <- T.numeric <|> unexpected "time"
  return $ makeTime h m
\end{code}

\subsection{Interval}

\begin{code}
interval :: Parser Interval
-- interval = Interval <$> (time <* T.symbol "-") *> time <?> "interval"
interval = do
  t1 <- try time <|> unexpected "start time"
  t2 <- try (T.symbol "-" *> time)
  return $ Interval t1 t2
\end{code}
