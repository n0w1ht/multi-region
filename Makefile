# multi-region: compile and test.
#
#   make compile         byte-compile every file; a warning is an error
#   make test            run the tests; pdf-tools, pdf-canvas and nov.el
#                        are imitated, so none of them is needed
#   make test-pdf-tools  check the pdf-tools that is installed against
#                        what multi-region-pdf-tools.el relies on; run it
#                        after updating pdf-tools
#   make check           all three
#   make info            rebuild the manual from doc/multi-region.org
#
# PDF_TOOLS is the folder of pdf-tools' .el files, and TABLIST that of
# tablist, which pdf-tools needs.  Without them the copies installed
# with package.el are used.

EMACS ?= emacs
PDF_TOOLS ?=
TABLIST ?=
SRC := $(filter-out %-test.el,$(wildcard multi-region*.el))
TESTS := multi-region-test multi-region-epub-test multi-region-pdf-test \
         multi-region-canvas-test

.PHONY: check compile test test-pdf-tools info clean

check: compile test test-pdf-tools

compile: clean
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC)
	rm -f *.elc

test:
	$(EMACS) -Q --batch -L . \
	  $(foreach t,$(TESTS),--eval "(require '$(t))") \
	  -f ert-run-tests-batch-and-exit

test-pdf-tools:
	$(EMACS) -Q --batch -L . $(if $(PDF_TOOLS),-L $(PDF_TOOLS)) \
	  $(if $(TABLIST),-L $(TABLIST)) \
	  -l multi-region-pdf-tools-test.el -f ert-run-tests-batch-and-exit

# doc/multi-region.org is the source; the .texi and .info made from it
# are committed, so that C-h R multi-region works without building.
info:
	cd doc && $(EMACS) -Q --batch multi-region.org \
	  --eval "(progn (require 'ox-texinfo) (org-texinfo-export-to-texinfo))"
	cd doc && makeinfo --no-split multi-region.texi -o multi-region.info
	cd doc && rm -f dir && install-info multi-region.info dir

clean:
	rm -f *.elc
