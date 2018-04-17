.PHONY: test doc typeset

.SUFFIXES :
.SUFFIXES : .tex .pdf

TEX2PDF := /Library/TeX/texbin/pdflatex
DOCDIR  := ./doc
SRC     := $(wildcard */*.hs)
DOCSRC  := $(wildcard */*.lhs)

all: test doc

test: $(SOURCES)
	stack test

doc: $(TEX2PDF) $(DOCSRC) $(DOCDIR)/*.tex index.pdf

$(TEX2PDF):
	brew cask install mactex

clean:
	cd $(DOCDIR) && rm -f *.aux *.out *.log *.toc

index.pdf: typeset
	$(TEX2PDF) index.tex
