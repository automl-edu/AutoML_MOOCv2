# ------------------------------------------------------------------------------
# Convenience Makefile for the BO chapter. Run from WD = 04_BO/.
# All LaTeX targets use `latexmk -cd` so work happens in the repo root, where
# main.tex and .latexmkrc live; outputs land next to main.tex.
# ------------------------------------------------------------------------------

ROOT := ..
MAIN := $(ROOT)/main.tex

.PHONY: build watch clean figs open help

build:        ## compile main.tex once (outputs $(ROOT)/main.pdf)
	latexmk -cd $(MAIN)

watch:        ## recompile on save (Ctrl-C to stop); open the PDF yourself
	latexmk -cd -pvc -view=none $(MAIN)

clean:        ## remove all latexmk-managed auxiliary files
	latexmk -cd -C $(MAIN)

figs:         ## regenerate figures from rsrc/*.R
	@for s in rsrc/*.R; do \
	  [ "$${s##*/}" = _setup.R ] && continue; \
	  echo "-- Rscript $$s"; \
	  Rscript $$s || exit $$?; \
	done

open:         ## open main.pdf in the default viewer
	@xdg-open $(ROOT)/main.pdf >/dev/null 2>&1 || open $(ROOT)/main.pdf

help:         ## list available targets
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
	  awk 'BEGIN{FS=":.*##"}; {printf "  %-8s %s\n", $$1, $$2}'
