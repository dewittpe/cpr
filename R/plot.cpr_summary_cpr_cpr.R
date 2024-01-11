#' Plotting Summaries of Control Polygon Reductions
#'
#' @param x a \code{cpr_summary_cpr_cpr} object
#' @param type response to plot by index
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... pass through
#'
#' @seealso \code{\link{plot.cpr_cpr}}, \code{\link{cpr}}
#' \code{\link{summary.cpr_cpr}}
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
#' s0   <- summary(cpr0)
#'
#' plot(s0, type = "rse")
#' plot(s0, type = "rss")
#' plot(s0, type = "loglik")
#' plot(s0, type = "wiggle")
#' plot(s0, type = "fdsc")
#' plot(s0, type = "Pr(>w_(1))")
#'
#' @export
plot.cpr_summary_cpr_cpr <- function(x, type = c("rse", "rss", "loglik", "wiggle", "fdsc", "Pr(>w_(1))"), from = 1, to, ...) {
  if (from < 1) {
    from <- 1
  }

  type <- match.arg(type, several.ok = FALSE)

  if (missing(to)) {
    to <- nrow(x)
  } else if (to > nrow(x)) {
    to <- nrow(x)
  }

  x$index <- seq_along(x[[type]])

  ggplot2::ggplot(subset(x, (x$index >= from) & (x$index <= to))) +
  ggplot2::theme_bw() +
  eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("index"), Y = as.name(type)))) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::xlab("Index")

}
