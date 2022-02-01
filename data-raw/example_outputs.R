# The following objects are used to reduce the time required for tests and
# building vignettes.


library(cpr)
initial_cp <- cp(log10(pdg) ~ bsplines(day, df = 54), data = spdg)
cpr_run <- cpr(initial_cp)
saveRDS(cpr_run, "./inst/example_objects/expected_cpr_run.rds")

