test_that("1D btensor matrix wraps x as a list with warning",
          {
            expect_warning(btensor(x = mtcars$hp))
          })

test_that("1D btensor matrix is constructed as expected",
          {

            bm <-
              btensor(x = list(mtcars$hp), iknots = list(c(100, 150)))

            mm <-
              model.matrix( ~ 0 +
                           splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE)
              )

            expect_equal(mm, unclass(bm), check.attributes = FALSE)

          })
test_that("2D btensor matrix is constructed as expected",
          {

            bm <-
              btensor(x = list(mtcars$disp, mtcars$hp),
                      iknots = list(numeric(0), c(100, 150)))

            mm <-
              model.matrix( ~ 0 +
                           splines::bs(mtcars$disp, intercept = TRUE) :
                           splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE)
              )

            expect_equal(mm, unclass(bm), check.attributes = FALSE)

          })

test_that("3D btensor matrix is constructed as expected",
          {

            bm <-
              btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
                      iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)))

            mm <-
              model.matrix( ~ 0 +
                           splines::bs(mtcars$disp, intercept = TRUE) :
                           splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE) :
                           splines::bs(mtcars$mpg, knots = c(12.2, 16.3, 21.9), intercept = TRUE))

            expect_equal(mm, unclass(bm), check.attributes = FALSE)

          })

test_that("bknots are constructed as exptected",
          {

            bm <-
              btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
                      iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)))

            expect_equal(
                         lapply(attr(bm, "bspline_list"), attr, which = "bknots"),
                         lapply(list(mtcars$disp, mtcars$hp, mtcars$mpg), range))

            expect_error(
              btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = c(12, 12, 3)),
              "is\\.list\\(bknots\\) is not TRUE"
              )

            expect_error(
              btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = list(12, 12, 3)),
              "all\\(sapply\\(bknots, length\\) == 2\\) is not TRUE"
              )

            expect_error(
              btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
                      bknots = list(c(12, 12), c(3, NA))),
              "length\\(bknots\\) == length\\(x\\) is not TRUE"
              )


          })
