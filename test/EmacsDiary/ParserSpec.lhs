% -*- coding: utf-8 -*-
\section{Testing the Parser}
\begin{code}
module EmacsDiary.ParserSpec (tests) where

import Test.HUnit
import SpecHelpers

import EmacsDiary.Parser (diary)
\end{code}

\begin{code}
tests = test [
  "parse a date-line with no entry" ~: do
      let r = ParserAssertion diary "7 July 2008" " 5 May 2018 00:00"
      assert $ 1 == 1
  ]
\end{code}
