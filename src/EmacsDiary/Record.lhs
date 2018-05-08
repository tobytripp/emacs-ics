\begin{code}
module EmacsDiary.Record where

import EmacsDiary.Interval
\end{code}

A \codeline{DiaryDate} specifies a collection of \codeline{DiaryEntry}s for a
given calendar date.

\begin{code}
data Diary = Diary [Record]

data Record = Record { date    :: Date
                     , entries :: [Entry] }
  deriving (Eq)

instance Show Record where
  show Record {date=d, entries=es} =
    (show d) ++ ": [" ++  unwords (map show es) ++ "]"

emptyRecord :: Date -> Record
emptyRecord d = Record d []
\end{code}

An \codeline{Entry} is the pairing of an event \emph{date} with a time,
description, and location.

\begin{code}
data EntryField = Description String
                | Location String
                deriving (Eq, Show)

data Entry = Entry { eventTime   :: Interval,
                     fields      :: [EntryField]}
  deriving (Eq, Show)

emptyEntry t = Entry t [Description ""]
\end{code}

Now, to emit ICS from a parsed \codeline{Record}:

\begin{code}
class Ics a where
  toIcs :: a -> String

instance Ics Diary where
  toIcs (Diary []) = ""
  toIcs (Diary rs) = "BEGIN:VCALENDAR\n"
    ++ (unlines $ map toIcs rs)
    ++ "END:VCALENDAR\n"

instance Ics Record where
  toIcs r@(Record _ []) = ""
  toIcs r@(Record _ es) = unlines $ map toIcs es


instance Ics Entry where
  toIcs e = "BEGIN:VEVENT\n"
    ++ "END:VEVENT\n"
\end{code}
