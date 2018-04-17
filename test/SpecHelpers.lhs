% -*- coding: utf-8 -*-
\section{Helper Functions}
\begin{code}
module SpecHelpers where
import Test.HUnit

import Text.Printf

import Data.Time.Clock
import Data.Time.Format

import Text.Parsec
import Text.Parsec.String
\end{code}

\subsection{Date Helpers}

Create a date-parser via partial application of @parseTimeOrError@:

\begin{code}
acceptOuterWhitespace = True

parseDate :: String -> UTCTime
parseDate = parseTimeOrError acceptOuterWhitespace defaultTimeLocale "%e %b %Y"

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%e %b %Y %R"
\end{code}

\subsection{Parser Testing Helpers}

\begin{code}
nullState = ()
testParse :: Parser a -> String -> String -> Either ParseError a
testParse p src = runParser p nullState src


-- assertParsesTo expected p input = do
--   let result = testParse p input
--   case result of
--     (Right actual) -> expected @=? show actual
--     (Left  error)  -> assertFailure
--       (printf "Failed to parse input `%s': %s" input (show error))
\end{code}

A Type for expressing parser assertions
\begin{code}
data ParserContext a =
  ParserContext { parser     :: Parser a
                  , input    :: String
                  , expected :: String
                  }

parses :: (Show a) => ParserContext a -> Assertion
parses pa =
  case testParse (parser pa) ("expected: " ++ (expected pa)) (input pa) of
    (Right actual) -> assertEqual "" (expected pa) (show actual)
    (Left error)   -> assertFailure (show error)

instance (Show a) => Testable (ParserContext a)
  where
    test = TestCase . parses

assertParser message expected parser input =
  parses pa
  where
    pa = ParserContext parser input expected
\end{code}

\subsection{Writing the Tests}

\begin{table}[hbt]
\caption{Test Types}
  \begin{tabular}{|rp{3.5in}|}
    \hline\hline
    \codeline{recordTests :: Test} & \\
    \codeline{test :: Testable t => t -> Test} &
    Provides a way to convert data into a @Test@ or set of @Test@. \\

    \codeline{(~:) :: Testable t => String -> t -> Test} &
    Creates a test from the specified @Testable@, with the specified label
    attached to it.

    Since @Test@ is @Testable@, this can be used as a shorthand way of attaching a
    S@TestLabel@ to one or more tests. \\

    \multicolumn{2}{|l|}{\codeline{assertEqual :: (Show a, Eq a) => String -> a -> a
    \-> Assertion}} \\

    \codeline{(@=?) :: (Show a, Eq a) => a -> a -> Assertion} &
    Asserts that the specified actual value is equal to the expected value (with
    the actual value on the left-hand side). \\
    \hline
  \end{tabular}
\end{table}
