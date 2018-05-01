\begin{code}
module EmacsDiary.Record where

import EmacsDiary.Interval
\end{code}

A \codeline{DiaryDate} specifies a collection of \codeline{DiaryEntry}s for a
given calendar date.

\begin{code}
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

data Entry = Entry { eventTime   :: Time,
                     fields      :: [EntryField]}
  deriving (Eq, Show)

emptyEntry t = Entry t [Description ""]
\end{code}
