# Makefile for the cpr package

PKG_VERSION = $(shell awk '/^Version:/{print $$2}' DESCRIPTION)
PKG_NAME    = $(shell awk '/^Package:/{print $$2}' DESCRIPTION)

SRC    = $(wildcard src/*.cpp)
RFILES = $(wildcard R/*.R)
MANS   = $(wildcard man/*.Rd)

.PHONY: fast all

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

fast: $(RFILES) $(SRC) DESCRIPTION
	R -e "devtools::document()"
	R CMD build --no-build-vignettes --no-manual .

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(RFILES) $(SRC) DESCRIPTION
	R -e "devtools::document()"
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	/bin/rm -f  $(PKG_NAME)_*.tar.gz
	/bin/rm -rf $(PKG_NAME).Rcheck
	cd vignettes; latexmk -C cpr-pkg.tex; latexmk -C bsplines.tex;
	/bin/rm -f vignettes/bsplinerefs.bib
	/bin/rm -f vignettes/bsplines-concordance.tex
	/bin/rm -f vignettes/bsplines.bbl
	/bin/rm -f vignettes/bsplines.synctex.gz
	/bin/rm -f vignettes/bsplines.tex
	/bin/rm -f vignettes/cpr-pkg-concordance.tex
	/bin/rm -f vignettes/cpr-pkg.synctex.gz
	/bin/rm -f vignettes/cpr-pkg.tex

