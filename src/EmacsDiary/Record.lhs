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

An \codeline{Entry} is the pairing of an event \emph{time} with a time and
location.

\begin{code}
type Description = String
type Address = String
data Entry = Entry { eventTime   :: Time,
                     description :: Description,
                     location    :: Address }
    deriving (Eq, Show)

emptyEntry t = Entry t "" ""
\end{code}
