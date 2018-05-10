\begin{code}
module EmacsDiary.Record where

import EmacsDiary.Interval
\end{code}

A \codeline{Diary} specifies a collection of \codeline{Records}s; a
\codeline{Record} is a collection of \codeline{Entries} for a
given calendar date.

\begin{code}
data Diary = Diary [Record]
data Record = Record { day :: Day
                     , entries :: Entries }
              deriving (Eq, Show)

data Day = Singular { date :: Date }
         | Repeating { weekDay :: WeekDay }
  deriving (Eq, Show)

empty :: Date -> Record
empty d = Record (Singular d) []

push :: Entry -> Record -> Record
push e (Record d es) = Record d (e:es)

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
  deriving (Eq, Show)

type Entries = [Entry]
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

entry :: Interval -> [String] -> Entry
entry t ds = Entry t $ map Description ds
\end{code}
