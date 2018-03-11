.SUFFIXES :
.SUFFIXES : .lhs .tex .pdf

TEX2PDF = /Library/TeX/texbin/pdflatex
SOURCES = $(wildcard */*.lhs)

all: $(TEX2PDF) doc.pdf
	open doc.pdf

$(TEX2PDF):
	brew cask install mactex

doc.pdf: doc.tex src/*.tex doc/*.tex

clean:
	rm -f *.aux *.out *.log *.toc

.tex.pdf:
	$(TEX2PDF) $<

.PHONY: all
