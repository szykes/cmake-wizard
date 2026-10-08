EMACS ?= emacs
LOAD   = -L . -L test
SRC    := $(wildcard cmake-wizard*.el)
TESTS  := $(wildcard test/*-test.el)
UNIT_TESTS := $(filter-out test/cmake-wizard-e2e-test.el,$(TESTS))

.PHONY: test unit-test e2e-test compile clean

test: unit-test e2e-test

unit-test:
	$(EMACS) -Q --batch $(LOAD) -l ert \
		$(foreach f,$(UNIT_TESTS),-l $(f)) \
		-f ert-run-tests-batch-and-exit

e2e-test:
	$(EMACS) -Q --batch $(LOAD) -l ert \
		-l test/cmake-wizard-e2e-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . --eval \
		"(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" \
		$(SRC)

clean:
	rm -f *.elc test/*.elc
