# targets
all: spdg.Rout

%.Rout: %.R
	R CMD BATCH --vanilla $<
	@touch $@

