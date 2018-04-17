\section{sepBy and endBy Combinators}

\begin{code}
import Text.ParserCombinators.Parsec
\end{code}

\begin{code}
csvFile = endBy line eol
line    = sepBy cell (char ',')
cell    = quotedCell <|> many (noneOf ",\n\r")
\end{code}

A CSV “cell” may be either a bare cell or a quoted cell.  Since a quoted cell
may, itself, contain quotes (doubled for escape) a @quotedCell@ is
\lstinline{many quotedChar}.

\begin{code}
quotedCell =
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell" -- see eol below
     return content
\end{code}

The function @quotedChar@ begins by consuming any character that is \emph{not}
itself a quote.  If it is a quoted character, the stream must be checked for a
second consecutive quote.  If so, a single quote mark is returned to the
result string.

\begin{quote}
 Notice that try in quotedChar on the right side of @<|>@. Recall that I said
 that try only has an effect if it is on the left side of @<|>@. This try does
 occur on the left side of a @<|>@, but on the left of one that must be within
 the implementation of many.

 This try is important. Let's say we are parsing a quoted cell, and are
 getting towards the end of it. There will be another cell following. So we
 will expect to see a quote to end the current cell, followed by a comma. When
 we hit quotedChar, we will fail the noneOf test and proceed to the test that
 looks for two quotes in a row. We'll also fail that one because we'll have a
 quote, then a comma. If we hadn't used try, we'd crash with an error at this
 point, saying that it was expecting the second quote, because the first quote
 was already consumed. Since we use try, this is properly recognized as not a
 character that's part of the cell, so it terminates the many quotedChar
 expression as expected. Lookahead has once again proven very useful, and the
 fact that it is so easy to add makes it a remarkable tool in Parsec.
\end{quote}

\begin{code}
quotedChar =
  noneOf "\""
  <|> try (string "\"\"" >> return '"')
\end{code}

Parsec also includes combinators for error handling and reporting.  A first
attempt at an @eol@ implementation that handles multiple line-ending styles might appear as:

\begin{code}
eol' =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
\end{code}

\begin{lstlisting}[language=sh]
λ> parseCSV "line1"
Left "(unknown)" (line 1, column 6):
unexpected end of input
expecting ",", "\n\r", "\r\n", "\n" or "\r"
\end{lstlisting}

The failure above is unclear and requires knowlege of the parser
implementation to debug fully.

The monad @fail@ function can be used to add messaging:

\begin{code}
eol' =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> fail "Couldn't find EOL"
\end{code}

This adds messaging to the result, but is still noisy and unclear:

\begin{lstlisting}[language=sh]
λ> parseCSV "line1"
Left "(unknown)" (line 1, column 6):
unexpected end of input
expecting ",", "\n\r", "\r\n", "\n" or "\r"
Couldn't find EOL
\end{lstlisting}

The Parsec @<?>@ operator is designed to help here.

\begin{quote}
It is similar to @<|>@ in that it first tries the parser on its left. Instead of trying another parser in the event of a failure, it presents an error message. Here's how we'd use it:
\end{quote}

\begin{code}
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"
\end{code}

This has a more pleasing result:

\begin{lstlisting}[language=sh]
λ> parseCSV "line1"
Left "(unknown)" (line 1, column 6):
unexpected end of input
expecting "," or end of line
\end{lstlisting}

\begin{quote}
The general rule of thumb is that you put a human description of what you're looking for to the right of @<?>@.
\end{quote}

\begin{code}
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
\end{code}
