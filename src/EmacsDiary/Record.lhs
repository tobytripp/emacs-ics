\begin{code}
module EmacsDiary.Record where

import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
\end{code}

A \codeline{DiaryDate} specifies a collection of \codeline{DiaryEntry}s for a
given calendar date.

\begin{code}
data DiaryDate = DiaryDate { date    :: Day
                           , entries :: [DiaryEntry] }
  deriving (Eq)

instance Show DiaryDate where
  show DiaryDate {date=d, entries=es} =
    (show d) ++ ": [" ++  unwords (map show es) ++ "]"
\end{code}

A \codeline{DiaryEntry} is the pairing of an event \emph{time} with a location.

\begin{code}
data DiaryEntry =
  DiaryEntry { eventTime :: UTCTime, location  :: String }
    deriving (Eq, Show)
\end{code}
