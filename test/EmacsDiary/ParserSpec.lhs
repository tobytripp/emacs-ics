% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser, many)
import Text.Printf (printf)

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
import EmacsDiary.Parser (diary, record, entry)
\end{code}

\begin{code}
assertParsesTo p input expected =
  case runParser p () input input of
    (Left e) -> assertFailure $ show e
    (Right actual) -> assertEqual ""
      expected
      actual
\end{code}

\begin{code}
tests = test [
  "parse a solitary entry" ~: do
      let input = "  14:30 entry\n"
      assertParsesTo entry input
        (Entry (I.timeFromList [14, 30]) [Description "entry"])
  ,

  "parse multi-element entry" ~: do
      let input = unlines [
            "  14:30 field1",
            "    field2",
            "    field3"
            ]
      assertParsesTo entry input
        (Entry (I.timeFromList [14, 30])
          [Description "field1",
            Description "field2",
            Description "field3"])
  ,

  "requires leading whitespace for entry" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "Other Text"
            ]
      assertParsesTo entry input
        (Entry (I.timeFromList [14, 30])
          [Description "field1",
            Description "field2"])
  ,

  "entry with combinator" ~: do
      let input = unlines [
            "  14:30 field1",
            "  15:30 field2"]
      assertParsesTo (many entry) input
        [(Entry (I.timeFromList [14, 30]) [Description "field1"]),
         (Entry (I.timeFromList [15, 30]) [Description "field2"])]
  ,

  "parse semicolon separator" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "    field3"
            ]
      assertParsesTo entry input
        (Entry (I.timeFromList [14, 30])
          [Description "field1",
            Description "field2",
            Description "field3"])
  ,

  "parse date and time with empty description produces error" ~: do
      let input = "7 July 2008 14:30\n"
      case runParser diary () input input of
        (Left e)  -> assert True
        (Right r) -> assertFailure
          (printf "parsing should have failed, but got '%s'" (show r))
  ,

  "record parsing returns a Record" ~: do
      let input = "7 July 2008\n"
      assertParsesTo diary input [(Record (I.fromDmy 7 7 2008) [])]
  ,

  "record parser" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      assertParsesTo record input
        (Record (I.fromDmy 7 7 2008)
          [Entry (I.timeFromList [14, 30])
            [Description "Work on parsers"]])
  ,

  "parse date and time with description" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      assertParsesTo diary input
        [(Record (I.fromDmy 7 7 2008)
           [Entry (I.timeFromList [14, 30])
             [Description "Work on parsers"]])]
  ,

  "parse multi-line entry" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "    With gusto!",
            ""]
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multi-line entry"
          [(Record (I.fromDmy 7 7 2008)
             [Entry (I.timeFromList [14, 30])
              [Description "Work on parsers",
               Description "With gusto!"]])]
          actual
  ,

  "parse multiple records" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "7 July 2008 15:15 Celebrate"]
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multiple records"
          [(Record (I.fromDmy 7 7 2008)
             [Entry (I.timeFromList [14, 30])
              [Description "Work on parsers"]]),
            (Record (I.fromDmy 7 7 2008)
             [Entry (I.timeFromList [15, 15])
              [Description "Celebrate"]])]
          actual
  ]
\end{code}
