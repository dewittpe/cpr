require(splines)

test_that("Equivalent Basis Matrix", 
          { 
            expect_equivalent(unclass(cpr::bsplines(0:10, iknots = c(2, 2.6, 7.8), bknots = c(0, 10), order = 4)),
                              unclass(splines::bs(0:10, knots = c(2, 2.6, 7.8), Boundary.knots = c(0, 10), intercept = TRUE)))
          }
)

