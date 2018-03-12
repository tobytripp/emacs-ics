\chapter{Parsec}

\section{Using Parsec  \cite[\href{http://book.realworldhaskell.org/read/using-parsec.html}{Ch.~16}]{rwh2008}}

\begin{code}
  import Text.ParserCombinators.Parsec
\end{code}

Input type is a sequence of characters, i.e., a Haskell
\lstinline{String}. \lstinline{String} is the same as \lstinline{[Char]}.  The
return value is \lstinline{[[String]]}; a list of a list of Strings.  We'll
ignore \lstinline{st} for now.

The \lstinline{do} block implies a \lstinline{Monad}.  \lstinline{GenParser}
is a parsing monad.

\lstinline{many} is a higher-order function that passes input repeatedly to
the function passed as its argument.  It collects the return values and
treturns them in a list.

\begin{code}
  csvFile :: GenParser Char st [[String]]
  csvFile =
    do result <- many line
       eof
       return result
\end{code}

A \lstinline{line} is a list of \lstinline{cells} followed by \lstinline{eol}.

\begin{code}
  line :: GenParser Char st [String]
  line =
    do result <- cells
       eol
       return result
\end{code}

\begin{code}
  cells :: GenParser Char st [String]
  cells =
    do first <- cellContent
       next  <- remainingCells
       return (first : next)
\end{code}

The choice operator, \lstinline{(<|>)}, tries the parser on the left and tries
The parser on the right if the left consumes no input.

\begin{code}
  remainingCells :: GenParser Char st [String]
  remainingCells =
    (char ',' >> cells) <|> (return [])
\end{code}

\begin{code}
  cellContent :: GenParser Char st String
  cellContent =
    many (noneOf ",\n")
\end{code}

\begin{code}
  eol :: GenParser Char st Char
  eol = char '\n'
\end{code}

\begin{code}
  parseCSV :: String -> Either ParseError [[String]]
  parseCSV input = parse csvFile "(unknown)" input
\end{code}
