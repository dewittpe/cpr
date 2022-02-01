# The following objects are used to reduce the time required for tests and
# building vignettes.

library(cpr)

# Object used in tests/testthat/test-cpr.R
initial_cp <- cp(log10(pdg) ~ bsplines(day, df = 54), data = spdg)
cpr_run <- cpr(initial_cp)
saveRDS(cpr_run, file.path("..", "inst", "example_objects", "expected_cpr_run.rds"))

# Object used in tests/testthat/test-cnr.R
initial_cn44 <- cn(log10(pdg) ~ btensor(list(day, age), df = list(12, 12)), data = spdg)
cnr_run <- cnr(initial_cn44)
saveRDS(cnr_run, file.path("..", "inst", "example_objects", "expected_cnr_run.rds"))
