EMACS = emacs -Q --batch
name = snakemake-mode
main_el :=  $(name).el
main_elc =  $(main_el)c
AUTOLOADS_FILE := $(name)-autoloads.el

all: elc autoloads

.PHONY: autoloads
autoloads: $(AUTOLOADS_FILE)

$(AUTOLOADS_FILE): $(main_el)
	@$(EMACS) --eval \
	"(let (make-backup-files) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"

.PHONY: clean
clean:
	$(RM) $(main_elc) $(AUTOLOADS_FILE)

.PHONY: elc
elc: $(main_elc)

.PHONY: help
help:
	@printf "\nMain targets:\n\n"
	@printf "  all                Byte compile and generate autoloads.\n"
	@printf "  autoloads          Generate $(AUTOLOADS_FILE).\n"
	@printf "  elc                Byte compile $(main_el).\n"
	@printf "\nOther:\n\n"
	@printf "  clean              Remove generated files.\n"
	@printf "  help               Print this message.\n"

%.elc: %.el
	@$(EMACS) -f batch-byte-compile $<
