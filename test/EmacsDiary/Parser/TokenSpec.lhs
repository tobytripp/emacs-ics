% -*- coding: utf-8 -*-
\begin{code}
module EmacsDiary.Parser.TokenSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)
import Text.Parsec.Error (errorMessages, messageString)
import EmacsDiary.Parser.Tokens
\end{code}

\begin{code}
tests = test [
  "parse empty line" ~: do
      let input = "\n"
      case runParser blanklines () "empty line" input of
        (Left e) -> assertFailure $ show e
        (Right actual) -> assertEqual "" "" actual
  ,
\end{code}

\begin{code}
  "parse line with no end-of-line" ~: do
      let input = ""
      case runParser blanklines () "empty line" input of
        (Left e) -> assertFailure $ show e
        (Right actual) -> assertEqual "" "" actual
  ]
\end{code}
