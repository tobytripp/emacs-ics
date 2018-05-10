% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser, many)
import Text.Printf (printf)

import qualified EmacsDiary.Record as R
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
        (R.entry (I.makeInterval 14 30) ["entry"])
  ,

  "parse multi-element entry" ~: do
      let input = unlines [
            "  14:30 field1",
            "    field2",
            "    field3"
            ]
      assertParsesTo entry input
        (R.entry (I.makeInterval 14 30) ["field1", "field2", "field3"])
  ,

  "requires leading whitespace for entry" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "Other Text"
            ]
      assertParsesTo entry input
        (R.entry (I.makeInterval 14 30) ["field1", "field2"])
  ,

  "entry with combinator" ~: do
      let input = unlines [
            "  14:30 field1",
            "  15:30 field2"]
      assertParsesTo (many entry) input
        [(R.entry (I.makeInterval 14 30) ["field1"]),
         (R.entry (I.makeInterval 15 30) ["field2"])]
  ,

  "parse semicolon separator" ~: do
      let input = unlines [
            "  14:30 field1; field2",
            "    field3"
            ]
      assertParsesTo entry input
        (R.entry (I.makeInterval 14 30) ["field1", "field2", "field3"])
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
      assertParsesTo diary input [R.empty (I.fromDmy 7 7 2008)]
  ,

  "record parser" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      assertParsesTo record input
        (R.push (R.entry (I.makeInterval 14 30) ["Work on parsers"])
          (R.empty (I.fromDmy 7 7 2008)))
  ,

  "parse date and time with description" ~: do
      let input = unlines ["7 July 2008 14:30 Work on parsers"]
      let e = R.entry (I.makeInterval 14 30) ["Work on parsers"]
      assertParsesTo diary input [R.push e (R.empty (I.fromDmy 7 7 2008))]
  ,

  "parse multi-line entry" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "    With gusto!",
            ""]
      let e = R.entry (I.makeInterval 14 30) ["Work on parsers", "With gusto!"]
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multi-line entry"
          [R.push e $ R.empty (I.fromDmy 7 7 2008)]
          actual
  ,

  -- "weekday entries" ~: do
  --     let input = "Wednesday 08:00 Wake up"
  --     let e = R.entry (I.makeInterval 8 0) ["Wake up"]
  --     assertParsesTo diary input [R.push e (R.Record (R.Repeating R.Wednesday) [])]
  --     ,

  "parse multiple records" ~: do
      let input = unlines [
            "7 July 2008 14:30 Work on parsers",
            "7 July 2008 15:15 Celebrate"]
      let e1 = R.entry (I.makeInterval 14 30) ["Work on parsers"]
      let e2 = R.entry (I.makeInterval 15 15) ["Celebrate"]
      case runParser diary () input input of
        (Left e)       -> assertFailure $ show e
        (Right actual) -> assertEqual "Multiple records"
          [(R.push e1 $ R.empty (I.fromDmy 7 7 2008)),
           (R.push e2 $ R.empty (I.fromDmy 7 7 2008))]
          actual
  ]
\end{code}
