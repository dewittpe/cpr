test_that("cpr is as expected",
          {
            initial_cp <- cp(log10(pdg) ~ bsplines(day, df = 54), data = spdg)
            cpr_run <- cpr(initial_cp)
            expected <- readRDS(system.file("example_objects", "expected_cpr_run.rds", package = "cpr"))
            expect_equal(cpr_run, expected)
          }
)

