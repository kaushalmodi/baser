# Makefile for basejump.el

ifdef INSIDE_EMACS
	EMACS := $(shell which emacs)
else
	EMACS ?= emacs
endif

TEST_DIR=$(shell pwd)/test

.PHONY: test

test:
	$(EMACS) --batch -l ert -L . -L $(TEST_DIR)/ -l all_tests.el -f ert-run-tests-batch-and-exit
