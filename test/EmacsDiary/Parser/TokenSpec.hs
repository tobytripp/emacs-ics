-- -*- coding: utf-8 -*-
module EmacsDiary.Parser.TokenSpec (tests) where

import Test.HUnit
import SpecHelpers

import Text.Parsec (runParser)
import Text.Parsec.Error (errorMessages, messageString)
import EmacsDiary.Parser.Tokens

tests = test [
  "parse empty line" ~: do
      let input = "\n"
      case runParser blankline () "empty line" input of
        (Left e) -> assertFailure $ show e
        (Right actual) -> assertEqual "" "" actual
  ,
  "parse line with no end-of-line" ~: do
      let input = ""
      let (Left error) = runParser blankline () "empty line" input
      let (src:err:msg:_) = lines . show $ error
      assertEqual "" "expecting new-line" $ msg
  ]
