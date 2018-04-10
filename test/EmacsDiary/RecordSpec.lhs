% -*- coding: utf-8 -*-
\subsection{The Preamble}

Declare the test module and export its tests.

\begin{code}
module EmacsDiary.RecordSpec (recordTests) where

import Test.HUnit
import Text.Parsec
import Text.Parsec.String
import Data.Time.Clock
import Data.Time.Format

import SpecHelpers

import EmacsDiary.Record
\end{code}

\begin{code}
recordTests = test [
  "create a DiaryEntry" ~: do
    let d = parseDate "7 July 2008"
    " 7 July 2008 Chicago, IL" @=? show (DiaryEntry d "Chicago, IL")
  ]
\end{code}
