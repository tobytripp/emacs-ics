% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (recordTests) where

import Test.HUnit
import SpecHelpers

import EmacsDiary.Record
\end{code}

\begin{code}
recordTests = test [
  "create a DiaryEntry" ~: do
    let (Right d) = parseTimeF "%e %b %Y" "7 July 2008"
    " 7 July 2008 Chicago, IL" @=? show (Entry d "" "Chicago, IL")
  ]
\end{code}
