% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser (diary)
\end{code}

\begin{code}
tests = test [
  "parse empty date-line: '7 July 2008'" ~: do
      assertParser "!!!" "[2008-07-07: []]" diary "7 July 2008"
  ,
  "parsing returns a Record" ~: do
      let (Right actual) = (runParser diary () "parser-spec" "7 July 2008")
      assertEqual "truth"
        [(Record (I.fromYmd 2008 7 7) [])]
        actual
  -- ,
  -- "parse date and time with empty description" ~: do
  --     assertParser "!!!" "2008-07-07 14:30" diary "7 July 2008 14:30"
  ]
\end{code}
