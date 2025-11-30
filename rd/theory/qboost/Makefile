TEX = qboost-paper.tex
PDF = $(TEX:.tex=.pdf)

.PHONY: all pdf clean

all: pdf

pdf:
	@if command -v latexmk >/dev/null 2>&1; then \
	  latexmk -pdf -interaction=nonstopmode -halt-on-error $(TEX); \
	else \
	  pdflatex -interaction=nonstopmode $(TEX); \
	  pdflatex -interaction=nonstopmode $(TEX); \
	fi

clean:
	@if command -v latexmk >/dev/null 2>&1; then \
	  latexmk -c $(TEX); \
	else \
	  rm -f *.aux *.log *.out *.toc *.fls *.fdb_latexmk; \
	fi

