% -*- coding: utf-8 -*-

\begin{code}
module EmacsDiary.Parser.Tokens where

import Text.Parsec (Column, sepBy, chainl, (<?>), manyTill, (<|>), many, many1, oneOf, noneOf, choice, try, many1, count)
import Text.Parsec.Char (letter, string, space, digit, anyChar, endOfLine)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (LanguageDef(..), emptyDef)
\end{code}

\begin{code}
lexer      = P.makeTokenParser diaryDef
numeric    = P.natural lexer
symbol     = P.symbol lexer
lexeme     = P.lexeme lexer
whitespace = P.whiteSpace lexer
\end{code}

\begin{code}
indent :: Column -> Parser String
indent i = do
  ss <- P.lexeme lexer $ count i space
  return ""
\end{code}

\begin{code}
digits :: Parser String
digits = P.lexeme lexer $ many1 digit
\end{code}

\begin{code}
word :: Parser String
word = P.lexeme lexer $ many1 letter
\end{code}

\begin{code}
line :: Parser String
line = many1 (noneOf ";\n\r")
\end{code}

\begin{code}
blanklines :: Parser String
blanklines = do {whitespace; return ""}
\end{code}

\begin{code}
keyword kw = P.symbol lexer kw <?> kw
\end{code}

\begin{code}
diaryDef :: LanguageDef st
diaryDef = emptyDef
           { P.commentLine    = "#"
           , P.caseSensitive  = True
           }
\end{code}
