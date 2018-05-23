module Main where

import Lib
import System.Environment

main :: IO ()
-- main = getArgs >>= parse >>= print
main = do
  (f:_) <- getArgs
  t <- now
  r <- (parseDiary t f)
  putStr $ toIcs r

-- parse [] = ""
-- parse fs = do
--   p <- mapM (parseDiary now) fs
--   return p
