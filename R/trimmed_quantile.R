#' Trimmed Quantiles
#'
#' For data \eqn{X = x_1, x_2, \ldots, x_n}{X = x1, x2, ..., xn}, with order
#' statistics \eqn{x_{(1)}, x_{(2)}, \ldots, x_{(r)}}{x(1), x(2), ..., x(r)}
#' return the quantiles for a trimmed data set, e.g.,
#' \eqn{\boldsymbol{X} \backslash \{x_{(1)}, x_{(r)}\}}{X \ {x(1), x(r)}} (trim = 1), or
#' \eqn{\boldsymbol{X} \backslash \{x_{(1)}, x_{(2)}, x_{(r-1)}, x_{(r)}\}}{X \ {x(1), x(2), x(r-1), x(r)}} (trim = 2).
#'
#' @param x a numeric vector
#' @param trim defaults to 1, omitting the min and the max
#' @param use_unique logical, if true (defaults), base the quantiles on unique
#' values, if false, base the quantiles on all data, after trimming.
#' @param ... other arguments to pass to stats::quantile
#'
#' @return a numeric vector, the return from \code{\link[stats]{quantile}}
#'
#' @seealso \code{\link[stats]{quantile}}
#'
#' @examples
#' trimmed_quantile(1:100, prob = 1:23 / 24, name = FALSE)
#'
#' # Warning
#' # trimmed_quantile(1:100, trim = .3, prob = 1:23 / 24, name = FALSE)
#'
#' # no warning
#' trimmed_quantile(1:100, trim = 3, prob = 1:23 / 24, name = FALSE)
#'
#' @export
trimmed_quantile <- function(x, trim = 1L, use_unique = TRUE, ...) {
  if (trim < 1) {
    trim <- 1L
    warning("Overruling trim less than 1 with trim = 1L")
  }
  if (floor(trim) != trim) {
    trim <- floor(trim)
    warning("Overruling non-integer trim with floor(trim)")
  }

  this_x <- x

  if (use_unique) {
    this_x <- unique(this_x)
  }

  for(i in seq(from = 1L, to = trim, by = 1)) {
    this_x <- this_x[!(this_x %in% range(this_x))]
  }

  stats::quantile(this_x, ...)
}

