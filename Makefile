.SUFFIXES :
.SUFFIXES : .lhs .tex .pdf

TEX2PDF := /Library/TeX/texbin/pdflatex
SOURCES := $(wildcard */*.lhs)

test: src/*.lhs src/*.hs test/*.hs
	stack test

all: $(TEX2PDF) doc/doc.pdf
	test

$(TEX2PDF):
	brew cask install mactex

%/doc.pdf: src/*.lhs doc/*.tex

clean:
	rm -f *.aux *.out *.log *.toc

.tex.pdf:
	$(TEX2PDF) -output-directory=doc/ $<

.PHONY: all test
