# Makefile for the cpr package

PKG_VERSION=$(shell awk '/^Version:/{print $$2}' DESCRIPTION)
PKG_NAME=$(shell awk '/^Package:/{print $$2}' DESCRIPTION)

RFILES:=$(wildcard R/*.R)

all: document build

document: $(RFILES)
	R -e "devtools::document()"

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz $(RFILES)
$(PKG_NAME)_$(PKG_VERSION).tar.gz: 
	R CMD build .

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	/bin/rm -f  $(PKG_NAME)_*.tar.gz
	/bin/rm -rf $(PKG_NAME).Rcheck
