\begin{code}
module EmacsDiary.Ics where

import Text.Printf (printf)

import EmacsDiary.Record
import EmacsDiary.Interval
\end{code}

Exporting parsed diary entries to @.ics@ files.

Begin with a type-class definition for types that can be exported.  An
instance of \codeline{Ics} is a type that can be represented as a
\codeline{String} component of an ICS record.

\begin{code}
class Ics a where
  toIcs :: a -> String
\end{code}

An Emacs \codeline{Diary} is a @VCALENDAR@ containing the @VEVENT@s of its
\codeline{Record}s.

\begin{code}
instance Ics Diary where
  toIcs (Diary []) = ""
  toIcs (Diary rs) = "BEGIN:VCALENDAR\n"
    ++ "VERSION:2.0\n"
    ++ (unlines $ map toIcs rs)
    ++ "END:VCALENDAR\n"
\end{code}

A Diary \codeline{Record} is simply the collection of its \codeline{Event}
representations.

\begin{code}
instance Ics Record where
  toIcs r@(Record _ []) = ""
  toIcs r@(Record _ es) = unlines $ map toIcs es
\end{code}

Each \codeline{Entry} is a @VEVENT@.

\begin{code}
instance Ics Entry where
  toIcs (Entry t fs) = "BEGIN:VEVENT\n"
    ++ icsUid t
    ++ toIcs t
    ++ edata fs
    ++ "END:VEVENT\n"
    where
      edata (s:es) = (summary s) ++ (unlines $ map show es)
      summary (Description s) = "SUMMARY:" ++ s ++ "\n"
      icsUid (Interval (Time _ t1) _) =
        printf "UID:ED%s\n" (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t1)
\end{code}

\codeline{Interval} emits a start and end.

\begin{code}
instance Ics Interval where
  toIcs (Interval start@(Time (Date _ createdAt) t) end) =
    printf "CREATED:%s\n" (iso8601 createdAt)
    ++
    printf "DTSTART:%s\nDTEND:%s\n"
    (iso8601 start)
    (iso8601 end)
  toIcs (Interval start@(Time (DayOfWeek _ createdAt) t) end) =
    printf "CREATED:%s\n" (iso8601 createdAt)
    ++
    printf "DTSTART:%s\nDTEND:%s\nRRULE:WEEKLY\n"
    (iso8601 start)
    (iso8601 end)
\end{code}
