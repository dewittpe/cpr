test_that("cnr is as expected",
          {
            initial_cn44 <- cn(log10(pdg) ~ btensor(list(day, age), df = list(12, 12)), data = spdg)
            cnr_run <- cnr(initial_cn44)
            expected <- readRDS("expected_cnr_run.rds")
            expect_equal(cnr_run, expected)
          }
)

