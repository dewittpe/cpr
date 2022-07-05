library(cpr)

test_warning <- function(x = NULL, msg = NULL) {
  stopifnot(!is.null(x))
  stopifnot(!is.null(msg))
  stopifnot(class(x) == c("simpleWarning", "warning", "condition"))
  stopifnot(x$message == msg)
  invisible(TRUE)
}

test_error <- function(x, msg = NULL) {
  stopifnot(!is.null(x))
  stopifnot(!is.null(msg))
  stopifnot(class(x) == c("simpleError", "error", "condition"))
  stopifnot(x$message == msg)
  invisible(TRUE)
}

################################################################################
# 1D btensor matrix wraps x as a list with warning
x <- tryCatch(btensor(x = mtcars$hp), warning = function(w){w})
test_warning(x, "wrapping x into a list.")

################################################################################
# 1D btensor matrix is constructed as expected

bm <-
  btensor(x = list(mtcars$hp), iknots = list(c(100, 150)))

mm <-
  model.matrix( ~ 0 +
               splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE)
  )

stopifnot(all.equal(mm, unclass(bm), check.attributes = FALSE))


################################################################################
# 2D btensor matrix is constructed as expected

bm <-
  btensor(x = list(mtcars$disp, mtcars$hp),
          iknots = list(numeric(0), c(100, 150)))

mm <-
  model.matrix( ~ 0 +
               splines::bs(mtcars$disp, intercept = TRUE) :
             splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE)
  )

stopifnot(all.equal(mm, unclass(bm), check.attributes = FALSE))

################################################################################
# 3D btensor matrix is constructed as expected

bm <-
  btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
          iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)))

mm <-
  model.matrix( ~ 0 +
               splines::bs(mtcars$disp, intercept = TRUE) :
             splines::bs(mtcars$hp, knots = c(100, 150), intercept = TRUE) :
             splines::bs(mtcars$mpg, knots = c(12.2, 16.3, 21.9), intercept = TRUE))

stopifnot(all.equal(mm, unclass(bm), check.attributes = FALSE))


################################################################################
# bknots are constructed as exptected

bm <-
  btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg),
          iknots = list(numeric(0), c(100, 150), c(12.2, 16.3, 21.9)))

stopifnot(
          all.equal(
                    lapply(attr(bm, "bspline_list"), attr, which = "bknots"),
                    lapply(list(mtcars$disp, mtcars$hp, mtcars$mpg), range)
          )
)

x <-
  tryCatch(
           btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = c(12, 12, 3))
           , error = function(e) {e})
test_error(x, "is.list(bknots) is not TRUE")

x <- tryCatch(
  btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = list(12, 12, 3))
  , error = function(e) {e})
test_error(x, "all(sapply(bknots, length) == 2) is not TRUE")

x <- tryCatch(btensor(x = list(mtcars$disp, mtcars$hp, mtcars$mpg), bknots = list(c(12, 12), c(3, NA))), error = function(e) {e})
test_error(x, "length(bknots) == length(x) is not TRUE")

################################################################################
