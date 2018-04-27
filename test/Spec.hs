import Test.HUnit
import Data.Time
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Either (isRight, fromLeft)
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitSuccess, exitFailure)

import qualified EmacsDiary.Parser.IntervalSpec as Interval
import qualified EmacsDiary.ParserSpec as PSpec
import qualified EmacsDiary.Parser.TokenSpec as Tokens

allTests = TestList [
  Interval.tests,
  Tokens.tests,
  PSpec.tests
  ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  case ((failures counts), (errors counts)) of
    (0, 0) -> exitSuccess
    otherwise -> exitFailure

-- Local Variables:
-- compile-command: "stack test"
-- End:
