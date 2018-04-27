-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.TokenSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)
import Text.Parsec.Error (errorMessages, messageString)
import EmacsDiary.Parser.Tokens

tests = test [
  "parse empty phrase" ~: do
      let input = "\n"
      let (Right actual) = runParser phrase () "empty phrase" input
      assertEqual "" "" actual
  ,
  "parse phrase with no end-of-line" ~: do
      let input = ""
      let (Left error) = runParser phrase () "empty phrase" input
      let (src:err:msg:_) = lines . show $ error
      assertEqual "" "expecting new-line or letter" $ msg
  ]
