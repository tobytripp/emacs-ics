\begin{code}
{-|
Description: Calendar records
-}
module EmacsDiary.Record (
  Diary(..),
  Record(..),
  Entry(..),
  EntryField(..),
  MetaData(..),

  empty,
  entry,
  push,

  metadata,
  descriptions
  ) where

import Data.List (mapAccumL)

import EmacsDiary.Interval (
  Date,
  Time,
  Interval
  )
\end{code}

A \codeline{Diary} specifies a collection of \codeline{Records}s; a
\codeline{Record} is a collection of \codeline{Entries} for a
given calendar date.

\begin{code}
-- | A Collection of Calendar dates.
data Diary = Diary [Record]
           | Failed { error :: String }
  deriving (Eq)

-- | A Collection of events for a particular Calendar date.
data Record = Record { day       :: Date
                     , entries   :: Entries }
  deriving (Eq, Show)

type Entries = [Entry]
data Entry = Entry { eventTime   :: Interval,
                     fields      :: [EntryField]}
  deriving (Eq)

data EntryField = Description String
                | Summary     String
                | Location    String
                deriving (Eq, Show)

data MetaData = MetaData { summary     :: EntryField
                         , description :: [EntryField]
                         , location    :: [EntryField]}
  deriving (Eq, Show)
\end{code}

\begin{code}
-- | Create an empty Calendar 'Record' on a given 'Date'.
empty :: Date -> Record

-- | Push a Calendar event onto the given 'Record'
push :: Entry -> Record -> Record

-- | Create a Calendar event in the given 'Interval' with given 'String's as
-- fields.
entry :: Interval                -- ^ when
      -> [String]                -- ^ what
      -> Entry
\end{code}

\begin{code}
-- | Return the 'Event' meta-data.
metadata :: Entry -> MetaData

-- | Return the 'Event' fields partitioned by description.
descriptions :: Entry
             -> ([String], [EntryField])            -- ^ semi-colon separated description
\end{code}

\subsection{Implementation}

\begin{code}
empty d = Record d []
push e (Record d es) = Record d (e:es)
\end{code}

An \codeline{Entry} is the pairing of an event \emph{date} with a time,
description, and location.

\begin{code}
instance Show Diary where
  show (Diary rs) = show rs
  show (Failed e) = e

instance Show Entry where
  show e@(Entry t fs) = (show t)
                      ++ " "
                      ++ show fs

entry t ds = Entry t $ map Description ds
\end{code}

\begin{code}
metadata (Entry _ fs) =
  (toMeta . mapAccumL segregate []) fs
  where
    segregate acc d@(Description _) = (acc ++ [d], [])
    segregate acc f = (acc, [f])

    toMeta ([], [])  = MetaData (Description "") [] []
    toMeta (d:[], f) = MetaData d [] (concat f)
    toMeta (d:ds, f) = MetaData d ds (concat f)

descriptions (Entry _ fs) =
  aggr . mapAccumL g [] $ fs
  where
    g acc (Description s) = (acc ++ [s], [])
    g acc x               = (acc, [x])
    aggr (acc, fs)        = (acc, concat fs)
\end{code}
