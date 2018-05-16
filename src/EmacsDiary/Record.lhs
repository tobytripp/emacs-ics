\begin{code}
{-|
Description: Calendar records
-}
module EmacsDiary.Record (
  Diary(..),
  Record(..),
  Day(..),
  Entry(..),

  EntryField(..),

  WeekDay(..), -- ^ Days of the week
  
  empty,
  entry,
  push
  ) where

import qualified EmacsDiary.Interval as I
\end{code}

A \codeline{Diary} specifies a collection of \codeline{Records}s; a
\codeline{Record} is a collection of \codeline{Entries} for a
given calendar date.

\begin{code}
-- | A Collection of Calendar dates.
data Diary = Diary [Record]

-- | A Collection of events for a particular Calendar date.
data Record = Record { day :: Day
                     , entries :: Entries }
              deriving (Eq, Show)

-- | A Calendar Day may occur on a specific 'Date' or repeat Weekly.
data Day = Singular  { date    :: I.Date }
         | Repeating { weekDay :: WeekDay }
  deriving (Eq, Show)

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
  deriving (Eq, Show)

type Entries = [Entry]
data Entry = Entry { eventTime   :: I.Interval,
                     fields      :: [EntryField]}
  deriving (Eq)

data EntryField = Description String
                | Location String
                deriving (Eq, Show)

-- | Create an empty Calendar 'Record' on a given 'Date'.
empty :: Day -> Record

-- | Push a Calendar event onto the given 'Record'
push :: Entry -> Record -> Record

-- | Create a Calendar event in the given 'Interval' with given 'String's as
-- fields.
entry :: I.Interval -> [String] -> Entry
\end{code}

\subsection{Implementation}

\begin{code}
empty d = Record d []
push e (Record d es) = Record d (e:es)
\end{code}

An \codeline{Entry} is the pairing of an event \emph{date} with a time,
description, and location.

\begin{code}
instance Show Entry where
  show (Entry t fs) = (show t) ++ " " ++ (unwords $ map show fs)

entry t ds = Entry t $ map Description ds
\end{code}
