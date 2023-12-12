# Makefile for the cpr package

PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/*.R)
RAWDATAR  = $(wildcard $(PKG_ROOT)/data-raw/*.R)

VIGNETTES  = $(PKG_ROOT)/vignettes/bsplines.Rmd

.PHONY: all check install clean covr

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

################################################################################
# build the tar ball
$(PKG_NAME)_$(PKG_VERSION).tar.gz: .document.Rout $(TESTS)
	R CMD build --md5 --compact-vignettes="gs+qpdf" $(build-options) $(PKG_ROOT)

.document.Rout: $(RFILES) $(SRC) $(RAWDATAR) $(VIGNETTES) $(PKG_ROOT)/DESCRIPTION
	if [ -e "$(PKG_ROOT)/data-raw/Makefile" ]; then $(MAKE) -C $(PKG_ROOT)/data-raw/; else echo "Nothing to do"; fi
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)', '$(BIOC)'))" \
		-e "devtools::document('$(PKG_ROOT)')"
	@touch $@

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

################################################################################
# Recipes for Vignettes
$(PKG_ROOT)/vignettes/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R
	R --vanilla --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"
	mv $(basename $<).Rmd $@

# for testing a vignette without having to install the package
%.html : vignettes/%.Rmd
	R -e "setwd('vignettes')"\
		-e "devtools::document()"\
		-e "rmarkdown::render('$(addsuffix .Rmd, $(basename $(notdir $<)))')"
	mv $(addsuffix .html, $(basename $<)) $@

################################################################################
covr :
	R --vanilla --quiet \
		-e 'library(covr)'\
		-e 'x <- package_coverage(type = "all", combine_types = FALSE)'\
		-e 'report(x[["tests"]], file = "covr-report-tests.html")'\
		-e 'report(x[["vignettes"]], file = "covr-report-vignettes.html")'\
		-e 'report(x[["examples"]], file = "covr-report-examples.html")'\
		-e 'x <- package_coverage(type = "all", combine_types = TRUE)'\
		-e 'report(x, file = "covr-report-all.html")'\

################################################################################
clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f src/*.o
	$(RM) -f src/*.so
	$(RM) -f vignettes/*.html
	$(RM) -f *.html

################################################################################
## End of File
################################################################################
