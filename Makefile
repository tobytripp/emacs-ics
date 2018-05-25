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
STACK_TARGET := ./.stack-work/install/x86_64-osx/lts-10.8/8.2.2/bin/emacs-ics-exe
EXE := ./dist/emacs-ics

DIARY_PATH := ~/Documents/Emacs/diary.gpg

all: build doc

import: $(EXE)
	gpg --output tmp/diary.txt --decrypt $(DIARY_PATH)
	$(EXE) tmp/diary.txt > tmp/emacs.ics
	open tmp/emacs.ics
	rm tmp/diary.*

$(EXE): build
	cp $(STACK_TARGET) $(EXE)

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
	cd $(DOCDIR) && \
        rm -f *.aux *.out *.log *.toc && \
        cd - && \
        rm -f tmp/*

index.pdf: typeset
	latexmk -pdf index.tex
