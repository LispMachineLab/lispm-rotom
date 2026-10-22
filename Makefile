EMACS ?= emacs
# lisp/ of lispm-agent, a checkout beside this one by default.  Rotom
# needs it not; with it on the load path, the tests of LispM's backends
# run too.
LISPM_AGENT ?= ../lisp_machine/lisp
LOAD_PATH = -L . $(foreach dir,$(wildcard $(LISPM_AGENT)),-L $(dir))

FILES = lispm-rotom.el lispm-rotom-ui.el

.PHONY: test compile

# The tests stand in for primitives such as read-string; without native
# trampolines for them, doing so starts no compiler.
test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-rotom-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
