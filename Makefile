.PHONY: test doc typeset

.SUFFIXES :
.SUFFIXES : .tex .pdf

TEXPATH := /Library/TeX/texbin
TEX2PDF := /Library/TeX/texbin/pdflatex
DOCDIR  := ./doc
SRC     := $(wildcard */*.hs)
DOCSRC  := $(wildcard */*.lhs)
SOURCES := $(SRC) $(DOCSRC)

HASKELL_STACK := /usr/local/bin/stack

all: build doc

build: test
	stack build

test: $(SOURCES) $(HASKELL_STACK)
	stack test

doc: $(TEX2PDF) $(DOCSRC) $(DOCDIR)/*.tex index.pdf

$(TEX2PDF):
	brew cask install mactex
$(HASKELL_STACK):
	brew install haskell-stack

clean:
	cd $(DOCDIR) && rm -f *.aux *.out *.log *.toc

index.pdf: typeset
	latexmk -pdf index.tex
