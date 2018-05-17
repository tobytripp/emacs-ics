\begin{code}
{-|
Description: Parsers for Calendar records
-}
module EmacsDiary.Parser.Record (
  record,                       -- ^ parse a Calendar date Record
  entry,                        -- ^ parse an individual Calendar Entry
  ) where
\end{code}

\subsection {Imports}

\begin{code}
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified EmacsDiary.Parser.Tokens as Tok
import qualified EmacsDiary.Parser.Interval as Interval
import EmacsDiary.Interval (ZonedTime)
import EmacsDiary.Record (
  Record(..),
  Entry(..),
  EntryField(..),

  empty,
  push)
\end{code}

\subsection {Parsers}

\begin{code}
record :: ZonedTime              -- ^ Current local-time
       -> Parser Record
entry  :: Record -> Parser Entry
\end{code}

\begin{code}
-- TODO: unwind/explain the (<$>) and (<*>) operators

record localt = do
  r <- empty <$> Interval.date localt
  let entryP = entry r
  entries <- (Tok.lexeme $ many entryP) <?> "Record"
  return $ foldr push r entries

entry (Record d _) =
  Entry <$> Interval.interval d <*> fieldsP <?> "Record.Entry"

fieldsP :: Parser [EntryField]
fieldsP = Tok.lexeme $ sepBy1 field sep
  where
    field = (description <|> location) <?> "entry field"
    sep = try (endOfLine *> Tok.indent 4)
      <|> (Tok.symbol ";")
      <?> "field-separator"

-- | An entry field representing part of the entry’s description
description = Description <$>
  (Tok.line <?> "description")

-- | An entry field representing the entry’s location
location = Location <$>
  (Tok.keyword "Location:" *>
   Tok.line <?> "location")

\end{code}
