
LOAD_PATH = -L ../transient/lisp -L .
EMACS = emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)
CURL := curl --silent

els := snakemake.el snakemake-mode.el
elcs := $(patsubst %.el, %.elc, $(els))
AUTOLOADS_FILE = snakemake-autoloads.el

.PHONY: all
all: $(elcs) $(AUTOLOADS_FILE)

.PHONY: test
test:
	@$(BATCH) -l snakemake-test \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

$(AUTOLOADS_FILE): $(els)
	@$(BATCH) -l autoload --eval \
	"(let ((make-backup-files nil) \
	       (generated-autoload-file \"$(CURDIR)/$@\")) \
	  (update-directory-autoloads \"$(CURDIR)/\"))"

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(elcs) $(AUTOLOADS_FILE)
