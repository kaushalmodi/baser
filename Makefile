# Makefile for baser.el

EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

.PHONY: test

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l all_tests.el -f ert-run-tests-batch-and-exit
