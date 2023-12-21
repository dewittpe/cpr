library(cpr)

test_warning <- function(x = NULL, msg = NULL) {
  stopifnot(!is.null(x))
  stopifnot(!is.null(msg))
  stopifnot(inherits(x, "warning"))
  stopifnot(identical(x$message,  msg))
  invisible(TRUE)
}

test_error <- function(x, msg = NULL) {
  stopifnot(!is.null(x))
  stopifnot(!is.null(msg))
  stopifnot(inherits(x, "error"))
  stopifnot(identical(x$message, msg))
  invisible(TRUE)
}

################################################################################
# 1D btensor matrix wraps x as a list with warning
e <- new.env()
with(e, {
  x <- tryCatch(btensor(x = mtcars$hp, bknots = list(c(50, 400))), warning = function(w){w})
  test_warning(x, "wrapping x into a list.")

  x <- suppressWarnings(
                        btensor(x = mtcars$hp, bknots = list(c(50, 400)))
  )
  x1 <- btensor(x = list(mtcars$hp), bknots = list(c(50, 400)))

  stopifnot(isTRUE(all.equal(x, x1, check.attributes = FALSE)))

})

################################################################################
# 1D btensor matrix is constructed as expected

e <- new.env()
with(e, {

  bm <-
    btensor(x = list(mtcars$hp), iknots = list(c(100, 150)), bknots = list(c(50,350)))

  mm <-
    model.matrix( ~ 0 +
                 splines::bs(mtcars$hp, knots = c(100, 150), Boundary.knots = c(50, 350), intercept = TRUE)
    )

  stopifnot(isTRUE(all.equal(mm, unclass(bm), check.attributes = FALSE)))

})

################################################################################
# 2D btensor matrix is constructed as expected

e <- new.env()
with(e, {
  bm <-
    btensor(x = list(mtcars$disp, mtcars$hp),
            iknots = list(numeric(0), c(100, 150)),
            bknots = list(c(70, 475), c(50, 350))
    )

  mm <-
    model.matrix( ~ 0 +
                 splines::bs(mtcars$disp, intercept = TRUE, Boundary.knots = c(70, 475)) :
                 splines::bs(mtcars$hp, knots = c(100, 150), Boundary.knots = c(50, 350), intercept = TRUE)
    )

  stopifnot(isTRUE(all.equal(mm, unclass(bm), check.attributes = FALSE)))

})

################################################################################
# 3D btensor matrix is constructed as expected

e <- new.env()
with(e, {
  bm <-
    btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
            iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)),
            bknots = list(c(70, 475), c(50, 350), c(10, 35))
    )

  mm <-
    model.matrix( ~ 0 +
                 splines::bs(mtcars$disp, intercept = TRUE, Boundary.knots = c(70, 475)) :
                 splines::bs(mtcars$hp, knots = c(100, 150), Boundary.knots = c(50, 350), intercept = TRUE) :
                 splines::bs(mtcars$mpg, knots = c(12.2, 16.3, 21.9), Boundary.knots = c(10, 35), intercept = TRUE))

  stopifnot(isTRUE(all.equal(mm, unclass(bm), check.attributes = FALSE)))
})


################################################################################
# bknots are constructed as exptected
e <- new.env()
with(e, {
  bm <-
    suppressWarnings(
       btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
               iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)))
    )

  stopifnot(isTRUE(
              all.equal(
                        lapply(attr(bm, "bspline_list"), attr, which = "bknots"),
                        lapply(list(mtcars$disp, mtcars$hp, mtcars$mpg), range)
              )
    )
  )
})

e <- new.env()
with(e, {
  x <-
    tryCatch(
             btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = c(12, 12, 3))
             , error = function(e) {e})
  test_error(x, "is.list(bknots) is not TRUE")
})

e <- new.env()
with(e, {
  x <- tryCatch(
    btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = list(12, 12, 3))
    , error = function(e) {e})
  test_error(x, "all(sapply(bknots, length) == 2) is not TRUE")
})

e <- new.env()
with(e, {
  x <- tryCatch(btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = list(c(12, 12), c(3, NA))), error = function(e) {e})
  test_error(x, "length(bknots) == length(x) is not TRUE")
})

################################################################################
# tests for behavior of the order argument
e <- new.env()
with(e, {
       test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm), order = c(3, 2)), error = function(e) e)
       test_error(test, "is.list(order) is not TRUE")

       test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm), order = list(c(3, 2))), error = function(e) e)
       test_error(test, "length(order) == length(x) is not TRUE")

       test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm), order = list(a = c(3, 2), 2)), error = function(e) e)
       test_error(test, "all(sapply(order, length) == 1) is not TRUE")

       x0 <- btensor(x = list(spdg$age, spdg$ttm),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(2.9, 4.1))

       x1 <- btensor(x = list(spdg$age, spdg$ttm),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(2, 4))

       stopifnot(isTRUE(all.equal(x0, x1, check.attributes = FALSE)))
})

################################################################################
# warning thrown when both iknots and df are provided
e <- new.env()
with(e, {
       test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm),
                                iknots = list(c(46, 48), c(-8)),
                                df = list(5, 5),
                                bknots = list(c(44, 53), c(-9, -1)),
                                order = list(3, 2)), warning = function(w) w)
       test_warning(test, "Both iknots and df defined, using iknots")

       x0 <- suppressWarnings(btensor(x = list(spdg$age, spdg$ttm),
                     iknots = list(c(46, 48), c(-8)),
                     df = list(5, 5),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(3, 2)))

       x1 <- btensor(x = list(spdg$age, spdg$ttm),
                     iknots = list(c(46, 48), c(-8)),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(3, 2))

       stopifnot(isTRUE(all.equal(x0, x1, check.attributes = FALSE)))

})

################################################################################
# error when length of x, iknots, bknots, order are not all the same
e <- new.env()
with(e, {

  test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm),
                          iknots = list(c(46, 48)),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(3, 2)), error = function(e) e)
  test_error(test, "Length of x, iknots, bknots, and order must be the same.")

  test <- tryCatch(btensor(x = list(spdg$age, spdg$ttm),
                          df = list(5),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(3, 2)), error = function(e) e)
  test_error(test, "length(df) == length(x) is not TRUE")

})

################################################################################
# expected construction when given df instead of iknots
e <- new.env()
with(e, {

       x0 <- btensor(list(spdg$age, spdg$ttm),
                          df = list(5, 5),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(4, 3))
       x1 <- btensor(list(spdg$age, spdg$ttm),
                     iknots = list(trimmed_quantile(spdg$age, probs = 0.5),
                                   trimmed_quantile(spdg$ttm, probs = 1:2/3)),
                          #df = list(5, 5),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(4, 3))

       stopifnot(isTRUE(all.equal(x0, x1, check.attributes = FALSE)))

})

e <- new.env()
with(e, {

       x0 <- tryCatch(btensor(list(spdg$age, spdg$ttm),
                          df = list(5, 5),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(4, 6)),
                      warning = function(w) w)
       test_warning(x0, "df being set to order")

       x0 <- btensor(list(spdg$age, spdg$ttm),
                          df = list(5, 5),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(4, 6))
       x1 <- btensor(list(spdg$age, spdg$ttm),
                     iknots = list(trimmed_quantile(spdg$age, probs = 0.5),
                                   numeric(0)),
                          bknots = list(c(44, 53), c(-9, -1)),
                          order = list(4, 6))

       stopifnot(isTRUE(all.equal(x0, x1, check.attributes = FALSE)))

})

e <- new.env()
with(e, {

       x0 <- btensor(list(spdg$age, spdg$ttm),
                     df = list(4, 5),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(4, 5))

       x1 <- btensor(list(spdg$age, spdg$ttm),
                     iknots = list(numeric(0), numeric(0)),
                     bknots = list(c(44, 53), c(-9, -1)),
                     order = list(4, 5))

       stopifnot(isTRUE(all.equal(x0, x1, check.attributes = FALSE)))

})

################################################################################
##                              printing method                               ##
e <- new.env()
with(e, {
  bm <-
    btensor(x = list(mtcars$disp, mtcars$hp),
            iknots = list(numeric(0), c(100, 150)),
            bknots = list(c(70, 475), c(50, 350))
    )

  stopifnot(identical(bm, print(bm)))

  bmcap <- capture.output(print(bm))
  stopifnot(identical(bmcap[1], "Tensor Product Matrix dims: [32 x 24]"))

})

################################################################################
##                                End of File                                 ##
################################################################################
