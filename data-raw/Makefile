# targets
FOR_TESTS  = ../inst/example_objects/expected_cpr_run.rds
FOR_TESTS += ../inst/example_objects/expected_cnr_run.rds

all: spdg.Rout example_objects.Rout

%.Rout: %.R
	R CMD BATCH --vanilla $<
	@touch $@

