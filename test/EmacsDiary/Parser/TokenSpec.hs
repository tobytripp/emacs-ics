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
      case runParser blanklines () "empty line" input of
        (Left e) -> assertFailure $ show e
        (Right actual) -> assertEqual "" "" actual
  ,

  "parse line with no end-of-line" ~: do
      let input = ""
      case runParser blanklines () "empty line" input of
        (Left e) -> assertFailure $ show e
        (Right actual) -> assertEqual "" "" actual
  ]
