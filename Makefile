.SUFFIXES :
.SUFFIXES : .lhs .tex .pdf

PROJECT := EmacsDiary
TEX2PDF := /Library/TeX/texbin/pdflatex
SOURCES := $(wildcard */*.lhs)

all: test doc

test: $(SOURCES) src/*.hs test/*.hs
	stack test

doc: $(TEX2PDF) $(SOURCES) test/*.lhs doc/*.tex
	touch doc/doc.tex
	make doc/doc.pdf

$(TEX2PDF):
	brew cask install mactex

%/doc.pdf: $(SOURCES) test/*.lhs doc/*.tex

clean:
	rm -f *.aux *.out *.log *.toc

.tex.pdf:
	$(TEX2PDF) -output-directory=doc/ $<

.PHONY: test doc
