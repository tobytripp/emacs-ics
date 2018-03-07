\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }

From http://book.realworldhaskell.org/read/using-parsec.html

\begin{code}
import Text.ParserCombinators.Parsec
\end{code}

Input type is a sequence of characters, i.e., a Haskell String.  String is the
same as [Char].  The return value is [[String]]; a list of a list of Strings.
sWe'll ignore st for now.

The

\begin{code}
csvFile :: GenParser Char st [[String]]
csvFile =
  do result <- many line
     eof
     return result
\end{code}

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
     next <- remainingCells
     return (first : next)
\end{code}

\begin{code}
remainingCells :: GenParser Char st [String]
remainingCells =
  (char ',' >> cells)
  <|> (return [])
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
parseCSV :: String -> Either ParserError [[String]]
parseCSV input = parse csvFile "(unknown)" input
\end{code}
