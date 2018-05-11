\begin{code}
module EmacsDiary.Record where

import qualified EmacsDiary.Interval as I
\end{code}

A \codeline{Diary} specifies a collection of \codeline{Records}s; a
\codeline{Record} is a collection of \codeline{Entries} for a
given calendar date.

\begin{code}
data Diary = Diary [Record]
data Record = Record { day :: Day
                     , entries :: Entries }
              deriving (Eq, Show)

data Day = Singular  { date    :: I.Date }
         | Repeating { weekDay :: WeekDay }
  deriving (Eq, Show)

empty :: Day -> Record
empty d = Record d []

epoch = empty (Singular $ I.epoch)

push :: Entry -> Record -> Record
push e (Record d es) = Record d (e:es)

dateOf :: Record -> I.Date
dateOf = date . day

onDate :: Int -> Int -> Integer -> Record
onDate d m y = empty . Singular $ I.date d m y

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

data Entry = Entry { eventTime   :: I.Interval,
                     fields      :: [EntryField]}
  deriving (Eq)

instance Show Entry where
  show (Entry t fs) = (show t) ++ " " ++ (unwords $ map show fs)


entry :: I.Interval -> [String] -> Entry
entry t ds = Entry t $ map Description ds
\end{code}
