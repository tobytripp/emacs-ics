% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (recordTests) where

import Test.HUnit
import SpecHelpers

import EmacsDiary.Record
import qualified EmacsDiary.Interval as I
\end{code}

\begin{code}
recordTests = test [
  "create a DiaryEntry" ~: do
    let (Right d) = parseTimeF "%e %b %Y" "7 July 2008"
    let t = I.mkInterval d Nothing
    " 7 July 2008 Chicago, IL" @=? show (Entry t [Location "Chicago, IL"])
  ]
\end{code}
