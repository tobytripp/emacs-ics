%% -*- coding: utf-8 -*-
\section{Parsing Dates and Times}

\begin{code}
module EmacsDiary.Parser.Interval where

import qualified EmacsDiary.Parser.Tokens as T
import EmacsDiary.Interval

import Text.Parsec ((<?>),
  many, string, count, spaces, digit, many1, choice, (<|>), try, sepBy1, unexpected)
import Text.Parsec.String (Parser)
import Data.Time.Calendar (fromGregorian)
\end{code}

\subsection{Date}

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
monthP = T.lexeme $ choice $ map keyValueParser months

date :: Parser Date
date = do
  d <- dayP
  m <- monthP
  y <- yearP
  return $ Date $ fromGregorian y m d
\end{code}

\subsection{Time}

\begin{code}
time :: Parser Time
time = timeFromList <$> (T.whitespace *> hms) <?> "time"
  where
    hms :: Parser [Integer]
    hms = sepBy1 T.numeric (T.symbol ":")
\end{code}
