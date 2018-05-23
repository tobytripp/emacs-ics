% -*- coding: utf-8 -*-

\begin{code}
import Test.HUnit
import Data.Time
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Either (isRight, fromLeft)
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitSuccess, exitFailure)
\end{code}

\begin{code}
import qualified EmacsDiary.Parser.IntervalSpec as Interval
import qualified EmacsDiary.ParserSpec          as Parser
import qualified EmacsDiary.Parser.TokenSpec    as Tokens
import qualified EmacsDiary.RecordSpec          as Records
import qualified EmacsDiary.IntervalSpec        as IntervalTypes
\end{code}

\begin{code}
allTests = TestList [
  Interval.tests,
  Tokens.tests,
  Parser.tests,
  Records.tests,
  IntervalTypes.tests
  ]
\end{code}

\begin{code}
main :: IO ()
main = do
  counts <- runTestTT allTests
  case ((failures counts), (errors counts)) of
    (0, 0) -> exitSuccess
    otherwise -> exitFailure
\end{code}

% Local Variables:
% compile-command: "stack test"
% End:
