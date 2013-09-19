.PHONY: all clean autoloads

EMACS=emacs

ELCFILES = $(addsuffix .elc, $(basename $(wildcard *.el)))

all: $(ELCFILES) autoloads

autoloads:
	@$(EMACS) -batch -q -no-site-file -L . -l autoload \
	--eval '(setq backup-inhibited t)' \
	--eval '(setq generated-autoload-file "$(PWD)/applescript-loaddefs.el")' \
	-f batch-update-autoloads .

%.elc : %.el
	@echo Compiling $<
	@$(EMACS) -batch -q -no-site-file -f batch-byte-compile $<

clean:
	@rm -f *.elc
