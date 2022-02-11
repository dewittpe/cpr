test_that("trimmed quantile warnings work",
          {
            expect_warning(trimmed_quantile(1:100, trim = 3.9, prob = 1:23 / 24, name = FALSE))
            expect_warning(trimmed_quantile(1:100, trim = -3.9, prob = 1:23 / 24, name = FALSE))
            expect_equivalent(suppressWarnings(trimmed_quantile(1:100, trim = 3.9, prob = 1:23 / 24, name = FALSE)),
                              quantile(4:97, prob = 1:23 / 24)
                              )
          })

test_that("trimmed quantile results are as expected",
          {
            x <- sample(runif(100, 0, 6), size = 500, replace = TRUE)
            ux <- unique(x)

            xt <- x[!(x %in% range(x))]
            uxt <- ux[!(ux %in% range(ux))]#-c(which.min(ux), which.max(ux))]

            expect_equivalent(trimmed_quantile(x), quantile(uxt))
            expect_equivalent(trimmed_quantile(x, use_unique = FALSE), quantile(xt))

          })
