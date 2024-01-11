#' Control Polygon Reduction Plots
#'
#' A wrapper around several ggplot2 calls to help evaluate results of a CPR run.
#'
#' @param x a \code{cpr_cpr} object
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... arguments passed to \code{plot.cpr_cp}
#'
#' @seealso \code{\link{plot.cpr_cp}}, \code{\link{cpr}}, \code{\link{cp}}
#'
#' @return a \code{ggplot} object
#'
#' @examples
#' set.seed(42)
#' x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
#' DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))
#'
#' initial_cp0 <-
#'   cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
#'      , data = DF
#'      , keep_fit = TRUE # default is FALSE
#'   )
#' cpr0 <- cpr(initial_cp0)
#'
#' plot(cpr0)
#' plot(cpr0, show_spline = TRUE, show_cp = FALSE, color = TRUE, from = 2, to = 4)
#'
#' @method plot cpr_cpr
#' @export
plot.cpr_cpr <- function(x, from = 1, to, ...) {

  if (from < 1) {
    from <- 1
  }

  if (missing(to)) {
    to <- 6
  } else if (to < from) {
    to <- from
  }

  nm <- deparse(substitute(x))

  expr <-
     parse(text = paste("plot(",
                        paste(paste0(nm, "[[", seq(from = from, to = to, by = 1), "]]"),
                              collapse = ", "),
                        ", ...)"))

  eval(expr)

}
