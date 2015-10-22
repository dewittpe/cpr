#' Trimmed Quantiles
#'
#' For data X = x1, x2, ..., xn, with order statistics x(1), x(2), ..., x(r) return
#' the quantiles for a trimmed data set, e.g., X \ {x(1), x(r)} (trim = 1), or 
#' X \ {x(1), x(2), x(r-1), x(r)} (trim = 2).
#'
#' @param x a numeric vector
#' @param trim defaults to 1, omitting the min and the max
#' @param use_unique logical, if true, base the quantiles on unique evalues, if
#' false (defualt) base the quantiles on all data, after trimming.   
#' @param ... other arguments to pass to stats::quantile
#'
#'
#' @examples
#' trimmed_quantile(1:100, prob = 1:23 / 24, name = F)
#'
#' # Warning
#' # trimmed_quantile(1:100, trim = .3, prob = 1:23 / 24, name = F)
#'
#' # no warning
#' trimmed_quantile(1:100, trim = 3, prob = 1:23 / 24, name = F)
#'
#' @export
trimmed_quantile <- function(x, trim = 1L, use_unique = FALSE, ...) { 
  if (trim < 1) {
    trim <- 1L
    warning("Overrulling trim less than 1 with trim < 1L")
  }
  if (floor(trim) != trim) {
    trim <- floor(trim)
    warning("Overrulling non-integer trim with floor(trim)")
  }

  this_x <- x
  for(i in seq(from = 1L, to = trim, by = 1)) { 
    this_x <- this_x[!(this_x %in% c(min(this_x), max(this_x)))]
  }

  if (use_unique) { 
    this_x <- unique(this_x)
  }

  stats::quantile(this_x, ...) 
}

