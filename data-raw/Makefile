# targets
all: spdg.Rout us_covid_cases.Rout

%.Rout: %.R
	R CMD BATCH --vanilla $<
	@touch $@

