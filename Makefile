# Makefile for the cpr package

PKG_VERSION=$(shell awk '/^Version:/{print $$2}' DESCRIPTION)
PKG_NAME=$(shell awk '/^Package:/{print $$2}' DESCRIPTION)

all: document vignettes build

background.Rnw:
	cat vignettes/background/00_0_preamble.Rnw        >  vignettes/background.Rnw
	cat vignettes/background/01_0_introduction.Rnw    >> vignettes/background.Rnw
	cat vignettes/background/02_0_b-splines-cps.Rnw   >> vignettes/background.Rnw
	cat vignettes/background/02_1_b-splines.Rnw       >> vignettes/background.Rnw
	cat vignettes/background/02_2_cps.Rnw             >> vignettes/background.Rnw
	cat vignettes/background/02_3_knot_operations.Rnw >> vignettes/background.Rnw
	cat vignettes/background/02_4_knot_influence.Rnw  >> vignettes/background.Rnw
	cat vignettes/background/A_appendix.Rnw           >> vignettes/background.Rnw

vignettes: background.Rnw
	R -e "devtools::build_vignettes()"

document: 
	R -e "devtools::document()"

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
$(PKG_NAME)_$(PKG_VERSION).tar.gz: 
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	/bin/rm -f  $(PKG_NAME)_*.tar.gz
	/bin/rm -rf $(PKG_NAME).Rcheck
