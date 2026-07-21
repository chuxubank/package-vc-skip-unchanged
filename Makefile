EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -L . \
		-l test/package-vc-skip-unchanged-test.el \
		-f ert-run-tests-batch-and-exit
