
-include config.mk

COMPAT_DIR ?= /dev/null
TRANSIENT_DIR ?= /dev/null

LOAD_PATH = -L $(COMPAT_DIR) -L $(TRANSIENT_DIR) -L .
EMACS = emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

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

.PHONY: sign-tar
sign-tar:
	tag="$$(git describe --abbrev=0)"; \
	object=$$(git archive --format tar \
		    --prefix "snakemake-mode-$${tag#v}/" "$$tag" | \
		  gpg --output - --armor --detach-sign | \
		  git hash-object -w --stdin); \
	git notes --ref=refs/notes/signatures/tar add -C "$$object" "$$tag"
