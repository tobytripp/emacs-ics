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
  "parse empty date-line: '7 July 2008'" ~: do
      assertParser "!!!" "2008-07-07" diary "7 July 2008"
  ]
\end{code}
