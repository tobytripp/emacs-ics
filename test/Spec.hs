import Test.HUnit
import Data.Time
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Either (isRight, fromLeft)
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitSuccess, exitFailure)

import EmacsDiary.Parser.IntervalSpec (intervalTests)
import qualified EmacsDiary.ParserSpec as PSpec

allTests = TestList [
  intervalTests,
  PSpec.tests]

main :: IO ()
main = do
  counts <- runTestTT allTests
  case ((failures counts), (errors counts)) of
    (0, 0) -> exitSuccess
    otherwise -> exitFailure

-- Local Variables:
-- compile-command: "stack test"
-- End:
