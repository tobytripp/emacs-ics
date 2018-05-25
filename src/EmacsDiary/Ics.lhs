\begin{code}
module EmacsDiary.Ics where

import Text.Printf (printf)
import Data.List (null, intersperse, intercalate)

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
  toIcs (Diary rs)
    = unlines $
      [ "BEGIN:VCALENDAR"
      , "VERSION:2.0"
      , (concat $ map toIcs rs)
      , "END:VCALENDAR"]
  toIcs (Failed e) = "!!! Invalid(?) Diary: " ++ e
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
  toIcs e@(Entry t fs) = "BEGIN:VEVENT\n"
    ++ uid t
    ++ toIcs t
    ++ (toIcs . metadata) e
    ++ "END:VEVENT"
    where
      uid (Interval (Time _ t1) _) =
        printf "UID:ED%s\n" (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t1)
\end{code}

\begin{code}
instance Ics MetaData where
  toIcs (MetaData (Description summary) descriptions locations) =
    (unlines . filter notEmpty) [
    printf "SUMMARY:%s" summary
    , description descriptions
    , location locations
    ]
    where
      notEmpty :: String -> Bool
      notEmpty s = length s > 0

      description :: [EntryField] -> String
      description [] = ""
      description ds = printf "DESCRIPTION:%s"
        (intercalate ";" [s | d <- ds, let (Description s) = d])

      location :: [EntryField] -> String
      location [] = ""
      location ls = printf "LOCATION:%s" $
        (intercalate ";" . map (\(Location s) -> replace ',' "\\," s)) ls

      replace :: Eq a => a -> [a] -> [a] -> [a]
      replace a b = concatMap (\c -> if c == a then b else [c])
\end{code}

\codeline{Interval} emits a start and end.

\begin{code}
instance Ics Interval where
  toIcs (Interval start@(Time (Date _ createdAt) t) end) =
    printf "CREATED:%s\n" (vCal createdAt)
    ++
    printf "DTSTART:%s\nDTEND:%s\n"
    (vCal start)
    (vCal end)
  toIcs (Interval start@(Time (DayOfWeek _ createdAt) t) end) =
    printf "CREATED:%s\n" (vCal createdAt)
    ++
    printf "DTSTART:%s\nDTEND:%s\nRRULE:FREQ=WEEKLY\n"
    (vCal start)
    (vCal end)
\end{code}
