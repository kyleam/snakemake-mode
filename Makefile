
LOAD_PATH ?=
BATCH = emacs -Q --batch $(LOAD_PATH)

name = snakemake-mode
main_el :=  $(name).el
main_elc =  $(main_el)c
AUTOLOADS_FILE := $(name)-autoloads.el

.PHONY: all
all: $(main_elc) $(AUTOLOADS_FILE)

.PHONY: test
test: $(main_elc)
	@$(BATCH) -L . -l test-snakemake-mode \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

$(AUTOLOADS_FILE): $(main_el)
	@$(BATCH) --eval \
	"(let ((make-backup-files nil) \
	       (generated-autoload-file \"$(CURDIR)/$@\")) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t))"

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) $(main_elc) $(AUTOLOADS_FILE)
