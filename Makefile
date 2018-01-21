
LOAD_PATH = -L .deps -L .
EMACS = emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)
CURL := curl --silent

els := snakemake.el snakemake-mode.el
elcs := $(patsubst %.el, %.elc, $(els))
AUTOLOADS_FILE = snakemake-autoloads.el

DASH_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
POPUP_URL := https://raw.githubusercontent.com/magit/magit-popup/master/magit-popup.el

.PHONY: all
all: $(elcs) $(AUTOLOADS_FILE)

.PHONY: test
test: | .deps
	@$(BATCH) -l snakemake-test \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

$(AUTOLOADS_FILE): $(els)
	@$(BATCH) --eval \
	"(let ((make-backup-files nil) \
	       (generated-autoload-file \"$(CURDIR)/$@\")) \
	  (update-directory-autoloads \"$(CURDIR)/\"))"

.deps:
	mkdir -p .deps
	$(CURL) $(DASH_URL) > .deps/dash.el
	$(CURL) $(POPUP_URL) > .deps/magit-popup.el

%.elc: %.el | .deps
	@$(BATCH) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(elcs) $(AUTOLOADS_FILE)

.PHONY: clean-all
clean-all: clean
	$(RM) -r .deps
