import Test.HUnit
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Either (isRight, fromLeft)
import EntryParser
import Text.Parsec (ParseError, parse, eof)
import Text.Parsec.String (Parser)
import System.Exit (exitSuccess, exitFailure)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%e %b %Y"

-- parseError :: Show a => Either ParseError a -> String
parseError = show . (fromLeft "?")

parserTests = test [
  "date to string"  ~: do
        let Right date = parseWithEof dateToken "1Jul2019"
        assertEqual "date token parse failure"
          "1 Jul 2019"
          (show date),
  "full valid date" ~: do
      let dateString = "1 Mar 2018"
      let when (Right date) = parseWithEof diaryEntry dateString
      let expected  = parseTime dateString
      assertEqual
        ("failed to parse: " ++ dateString)
        "01 Mar 2018"
        (formatTime defaultTimeLocale "%d %b %Y" expected)
  ]

main :: IO ()
main = do
  counts <- runTestTT parserTests
  case ((failures counts), (errors counts)) of
    (0, 0) -> exitSuccess
    otherwise -> exitFailure

-- Local Variables:
-- compile-command: "stack test"
-- End:
