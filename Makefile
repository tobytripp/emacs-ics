.PHONY: test doc typeset

.SUFFIXES :
.SUFFIXES : .tex .pdf

TEXPATH := /Library/TeX/texbin
TEX2PDF := /Library/TeX/texbin/pdflatex
DOCDIR  := ./doc
SRC     := $(wildcard */*.hs)
DOCSRC  := $(wildcard */*.lhs)
SOURCES := $(SRC) $(DOCSRC)

all: build doc

build: test
	stack build

test: $(SOURCES)
	stack test

doc: $(TEX2PDF) $(DOCSRC) $(DOCDIR)/*.tex index.pdf

$(TEX2PDF):
	brew cask install mactex

clean:
	cd $(DOCDIR) && rm -f *.aux *.out *.log *.toc

index.pdf: typeset
	latexmk -pdf index.tex
