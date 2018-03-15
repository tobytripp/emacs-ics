module EmacsDiary.Parser.IntervalSpec
        ( intervalTests
        ) where

import Test.HUnit
import EmacsDiary.Parser.Interval

import Text.Parsec
import Text.Parsec.String
import Data.Time.Clock
import Data.Time.Format

testParse :: Parser a -> String -> Either ParseError a
testParse p s = runParser p () "Spec.hs" s

parseDateTime :: String -> UTCTime
parseDateTime = parseTimeOrError True defaultTimeLocale "%e %b %Y %R"

parseDate :: String -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale "%e %b %Y"

intervalTests = test [
  "parse time on given day" ~: do
    let d = parseDate "5 May 2018"
    let (Right actual) = testParse (time d) "17:18"
    " 5 May 2018 17:18" @=? -- assertEqual
      (formatTime defaultTimeLocale "%e %b %Y %R" actual)
    ,
  "parsing valid date" ~: do
    let input = "5 May 2018"
    let expected = parseDate input
    let (Right actual) = testParse date input
    " 5 May 2018 00:00" @=?
      (formatTime defaultTimeLocale "%e %b %Y %R" actual)
  ]
