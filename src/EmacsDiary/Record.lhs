\begin{code}
module EmacsDiary.Record where

import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
\end{code}

\begin{code}
data DiaryDate = DiaryDate { date    :: Day
                           , entries :: [DiaryEntry] }
  deriving (Eq)

instance Show DiaryDate where
  show DiaryDate {date=d, entries=es} =
    (show d) ++ ": [" ++  unwords (map show es) ++ "]"
\end{code}

\begin{code}
data DiaryEntry =
  DiaryEntry { eventTime :: UTCTime, location  :: String }
    deriving (Eq, Show)
\end{code}
