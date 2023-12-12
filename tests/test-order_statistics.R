library(cpr)


################################################################################
# Simple tests - similar to Example 1 for order_statistics

e <- new.env()
with(e, {
  simulated_data <- matrix(rnorm(n = 54 * 15000), ncol = 54)

  # find all the minimums for each of the simulated samples of size 54
  mins <- apply(simulated_data, 1, min)

  # get the density values
  x <- seq(-5, 0, length.out = ncol(simulated_data))
  d <- d_order_statistic(x, n = 54, j = 1, distribution = "norm")
  p <- p_order_statistic(q = x,  n = 54, j = 1, distribution = "norm")

  stopifnot(inherits(d, "numeric"))
  stopifnot(length(d) == length(x))
  stopifnot(inherits(p, "numeric"))
  stopifnot(length(p) == length(x))

  # plot the histogram and density -- This part of the example and should be
  # visually a good match
  if (interactive()) {
    par(mfrow = c(1, 2))

    hist(mins, freq = FALSE, breaks = 100)
    points(x, d, type = "l", col = "red")

    plot(ecdf(mins))
    points(x, p, col = "red")
  }
})
rm(e)

################################################################################
# A NA is returned when in the j or x,q arguments
e <- new.env()
with(e, {
  d <- d_order_statistic(-3, n = 5, j = c(1, NA, 2), distribution = "norm")
  p <- p_order_statistic(-3, n = 5, j = c(1, NA, 2), distribution = "norm")
  stopifnot( is.na(d) == c(FALSE, TRUE, FALSE) )
  stopifnot( is.na(p) == c(FALSE, TRUE, FALSE) )

  d <- d_order_statistic(c(-3, 2, NA), n = 5, j = c(1, NA, 2), distribution = "norm")
  p <- p_order_statistic(c(-3, 2, NA), n = 5, j = c(1, NA, 2), distribution = "norm")
  stopifnot( is.na(d) == c(FALSE, TRUE, TRUE) )
  stopifnot( is.na(p) == c(FALSE, TRUE, TRUE) )
})
rm(e)


################################################################################
# j-order statistics
# if x or q, and j have the same length, then a return

e <- new.env()
with(e, {
  x <- j <- c(1, 2)
  d <- d_order_statistic(x = x, n = 2, j = j, distribution = "norm")
  p <- p_order_statistic(q = x, n = 2, j = j, distribution = "norm")
  stopifnot(inherits(d, "numeric"))
  stopifnot(length(d) == length(x))
  stopifnot(inherits(p, "numeric"))
  stopifnot(length(p) == length(x))
})
rm(e)

################################################################################
# verify errors are thrown when inputs are of unequal length or otherwise not as
# expected

e <- new.env()
with(e, {
  d <- tryCatch(d_order_statistic(x = x, n = 4:6, j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(d, "simpleError"))
  stopifnot(d$message == "length(n) == 1 is not TRUE")
  p <- tryCatch(p_order_statistic(q = x, n = 4:6, j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(p, "simpleError"))
  stopifnot(p$message == "length(n) == 1 is not TRUE")
})
rm(e)

e <- new.env()
with(e, {
  d <- tryCatch(d_order_statistic(x = x, n = numeric(0), j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(d, "simpleError"))
  stopifnot(d$message == "length(n) == 1 is not TRUE")

  p <- tryCatch(p_order_statistic(q = x, n = numeric(0), j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(p, "simpleError"))
  stopifnot(p$message == "length(n) == 1 is not TRUE")
})
rm(e)

e <- new.env()
with(e, {
  d <- tryCatch(d_order_statistic(x = 0, n = NA_real_, j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(d, "simpleError"))
  stopifnot(d$message == "!is.na(n) is not TRUE")

  p <- tryCatch(p_order_statistic(q = 0, n = NA_real_, j = 2, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(p, "simpleError"))
  stopifnot(p$message == "!is.na(n) is not TRUE")
})
rm(e)

e <- new.env()
with(e, {
  d <- tryCatch(d_order_statistic(x = 0, n = 10, j = 11, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(d, "simpleError"))
  stopifnot(d$message == "n >= stats::na.omit(j) is not TRUE")

  p <- tryCatch(p_order_statistic(q = 0, n = 10, j = 11, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(p, "simpleError"))
  stopifnot(p$message == "n >= stats::na.omit(j) is not TRUE")
})
rm(e)

e <- new.env()
with(e, {
  d <- tryCatch(d_order_statistic(x = 0, n = 10, j = -1, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(d, "simpleError"))
  stopifnot(d$message == "stats::na.omit(j) >= 1 is not TRUE")

  p <- tryCatch(p_order_statistic(q = 0, n = 10, j = -1, distribution = "norm")
                , error = function(e) e)
  stopifnot(inherits(p, "simpleError"))
  stopifnot(p$message == "stats::na.omit(j) >= 1 is not TRUE")
})
rm(e)


################################################################################
#                                 End of File                                  #
################################################################################
