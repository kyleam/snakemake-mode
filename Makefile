
LOAD_PATH ?=
BATCH = emacs -Q --batch $(LOAD_PATH)

els := snakemake.el snakemake-mode.el
elcs := $(patsubst %.el, %.elc, $(els))
AUTOLOADS_FILE = snakemake-autoloads.el

.PHONY: all
all: $(elcs) $(AUTOLOADS_FILE)

.PHONY: test
test:
	@$(BATCH) -L . -l snakemake-test \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

$(AUTOLOADS_FILE): $(main_el)
	@$(BATCH) --eval \
	"(let ((make-backup-files nil) \
	       (generated-autoload-file \"$(CURDIR)/$@\")) \
	  (update-directory-autoloads \"$(CURDIR)/\"))"

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(elcs) $(AUTOLOADS_FILE)
