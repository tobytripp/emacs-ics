% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)
import Text.Printf (printf)

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

  "parse a solitary entry" ~: do
      let input = "  14:30 entry\n"
      case runParser entry () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual
          ""
          (Entry (I.timeFromList [14, 30]) [Description "entry"])
          actual
  ,

  "parse date and time with empty description produces error" ~: do
      let input = "7 July 2008 14:30\n"
      case runParser diary () input input of
        (Left e)  -> assert True
        (Right r) -> assertFailure
          (printf "parsing should have failed, but got '%s'" (show r))
  ,

  "parse date and time with description" ~: do
      let input = "7 July 2008 14:30 Work on parsers\n"
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Non-empty Entry"
          [(Record (I.fromYmd 2008 7 7)
             [Entry (I.timeFromList [14, 30])
               [Description "Work on parsers"]])]
          actual
  ,

  "parse multi-line entry" ~: do
      let input = "7 July 2008 14:30 Work on parsers\n  With gusto!\n"
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multi-line entry"
          [(Record (I.fromYmd 2008 7 7)
             [Entry (I.timeFromList [14, 30])
              [Description "Work on parsers",
               Description "With gusto!"]])]
          actual

  ]
\end{code}
