CASK ?= cask
EMACS ?= emacs

all: test

install:
	${CASK} install

test:
	${CASK} exec buttercup -L . -L tests

.PHONY: all install test
