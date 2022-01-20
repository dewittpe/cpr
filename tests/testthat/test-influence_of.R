test_that("Influcnce is as expected",
          {
            bmat <- bsplines(x      = seq(0, 6, length = 500),
                             iknots = c(1.0, 1.5, 2.3, 4.0, 4.5))
            theta <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
            omit_xi <- influence_of(cp(bmat, theta), c(6, 8))
            expect_equal(omit_xi$weight$w[1], 0.5391355923)   #sprintf("%.10f", omit_xi$weight$w[1])
            expect_equal(omit_xi$weight$w[2], 0.2775424520)
          }
)
