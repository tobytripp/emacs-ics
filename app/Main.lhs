% -*- coding: utf-8 -*-

\begin{code}
module Main where

import Lib
import System.Environment
\end{code}

\begin{code}
main :: IO ()
main = do
  (f:_) <- getArgs
  t <- now
  r <- (parseDiary t f)
  putStr $ toIcs r
\end{code}
