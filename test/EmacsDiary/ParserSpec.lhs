% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser (diary, entry)
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
  ,
  "parse a solitary empty entry" ~: do
      let input = "  14:30"
      case runParser entry () "empty entry" input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual ""
          (Entry (I.timeFromList [14, 30]) "" "")
  ,

  "parse date and time with empty description" ~: do
      let input = "7 July 2008 14:30"
      case runParser diary () "empty description" input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Empty Entry"
          [(Record (I.fromYmd 2008 7 7)
             [Entry (I.timeFromList [14, 30]) "" ""])]
          actual
  -- ,
  -- "parse single-line entry" ~: do
  --     let input = "28 April 2018  13:10 Work on parsers"
  --     let (Right actual) = runParser diary () "single-line" input
  --     assertEqual ""
  --       [(Record (I.fromYmd 2018 4 28)
  --         [Entry (I.timeFromList [13, 10])
  --           "Work on parsers"
  --           ""])]
  --       actual
  ]
\end{code}
