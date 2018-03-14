\begin{code}
module EntryParser
  (diaryEntry
  , dateToken
  , DiaryEntry
  , when
  ) where
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
\end{code}

\begin{code}
data DiaryEntry = When UTCTime
                | Where String
                deriving (Eq, Show)
when :: DiaryEntry -> UTCTime
when (When d) = d

data SimpleDate = SimpleDate { day   :: Integer
                             , month :: String
                             , year  :: Integer }
                             deriving (Eq)
instance Show SimpleDate where
  show sd = (show $ day sd) ++
            " " ++
            (month sd) ++
            " " ++
            (show $ year sd)
\end{code}

\begin{code}
lexer = P.makeTokenParser emptyDef
\end{code}

\begin{code}
dayP :: Parser Integer
dayP = P.natural lexer

word = many1 letter

monthP :: Parser String
monthP = word

yearP :: Parser Integer
yearP = P.natural lexer
\end{code}

\begin{code}
definitely :: Parser (Maybe a) -> Parser a
definitely p = do
  m <- p
  case m of
    Just t  -> return t
    Nothing -> unexpected "date parse failure"

dateToken :: Parser SimpleDate
dateToken = SimpleDate <$> dayP <*> monthP <*> yearP

date :: Parser (Maybe UTCTime)
date = toTime <$> dateToken
  where
    toTime :: SimpleDate -> Maybe UTCTime
    toTime d  = parseTimeM True defaultTimeLocale "%d %b %Y" (show d)

diaryEntry :: Parser DiaryEntry
diaryEntry = When <$> definitely date
\end{code}


%% Local Variables:
%% compile-command: "stack test"
%% End:
