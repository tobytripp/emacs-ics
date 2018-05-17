% -*- coding: utf-8 -*-
\section{Helper Functions}
\begin{code}
module SpecHelpers where
import Test.HUnit

import Text.Printf

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime

import Text.Parsec
import Text.Parsec.String
import Data.Time.LocalTime (utc)

import qualified EmacsDiary.Interval as Interval
\end{code}

\subsection{Date Helpers}

TimeZones

\begin{code}
cdt = hoursToTimeZone (-5)
\end{code}

\subsection{Parser Testing Helpers}

\begin{code}
assertParsesTo :: (Eq expected, Show expected)
  => Parsec SourceName () expected -- ^ Parser under test
  -> String                       -- ^ input
  -> expected                     -- ^ expected parser output
  -> IO ()

assertParsesTo p input expected =
  case runParser p nullState input input of
    (Left e) -> assertFailure $ show e
    (Right actual) -> assertEqual "" expected actual
  where
    nullState = ()
\end{code}


\subsection{Writing the Tests}

\begin{table}[hbt]
\caption{Test Types}
  \begin{tabular}{|rp{3.5in}|}
    \hline\hline
    \codeline{tests :: Test} & \\
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
