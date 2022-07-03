initial_cn <- cn(log10(pdg) ~ btensor(list(day, age), df = list(8, 9)), data = spdg)
cnr_run <- cnr(initial_cn)
s <- summary(cnr_run)

test_that("cnr is as expected",
          {

            expect_equal(s$index, seq(1, 10, by = 1))
            expect_equal(s$dfs, c(16, 20, 24, 28, 35, 42, 49, 56, 64, 72))
            expect_equal(s$loglik, c(-9512.58567894537, -8951.50689783384, -8883.50289144705, -8831.44127623629, -8821.98275147419, -8812.9058638868, -8809.40727072676, -8803.01821247239, -8801.29710869293, -8798.12072311303))
            expect_equal(s$rmse, c(0.356048723563453, 0.34802886989375, 0.34706920155575, 0.346336300041903, 0.346203313136765, 0.346075740068024, 0.346026580890623, 0.345936825439751, 0.345912650826547, 0.345868039769884))


            # testing iknots1
            expected_iknots <- numeric(0)
            expect_equal(object = s$iknots1[[1L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.173913043478261))
            expect_equal(object = s$iknots1[[2L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.266666666666667))
            expect_equal(object = s$iknots1[[3L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.45))
            expect_equal(object = s$iknots1[[4L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots1[[5L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots1[[6L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots1[[7L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.722222222222222))
            expect_equal(object = s$iknots1[[8L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots1[[9L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots1[[10L]], expected = expected_iknots)

            # testing iknots2
            expected_iknots <- numeric(0)
            expect_equal(object = s$iknots2[[1L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots2[[2L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots2[[3L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots2[[4L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 47.1793810382666))
            expect_equal(object = s$iknots2[[5L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 48.0744195779572))
            expect_equal(object = s$iknots2[[6L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 50.8368384038569))
            expect_equal(object = s$iknots2[[7L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, numeric(0)))
            expect_equal(object = s$iknots2[[8L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 48.828610131402))
            expect_equal(object = s$iknots2[[9L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 49.7185266265909))
            expect_equal(object = s$iknots2[[10L]], expected = expected_iknots)

          }
)

test_that("cnr plotting",
          {
            expect_error(plot(cnr_run, type = "not-a-type"))
          })

################################################################################
### #
### # The following code is helpful for creating the tests for the iknots.
### #
### # define a function for finding unique values between numeric
### # vectors with a tollerance
###
###
### find_unique <- function(x, y, tol = sqrt(.Machine$double.eps)) {
###   lwr <- y - tol
###   upr <- y + tol
###   z <- sapply(x, function(xx) { any(lwr < xx & xx < upr )})
###   x[!z]
### }
###
### expected_iknots <- numeric(0)
### # create the expressions and print them to the console
### for(i in 2:(length(s$iknots2))) {
###   d <- (find_unique(s$iknots2[[i]], expected_iknots))
###   e1 <- substitute(expected_iknots <- sort(c(expected_iknots, dd)), list(dd = d))
###   e2 <- substitute(expect_equal(object = s$iknots2[[ii]], expected = expected_iknots), list(ii = i))
###   print(e1)
###   eval(e1)
###   print(e2)
### }
###
################################################################################
###                               End of File                                ###
################################################################################

