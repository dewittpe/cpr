#' Control Net Reduction Plots
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#'
#' @method plot cpr_cnr
#' @rdname cnr_diagnostics
#' @export
#' @param x a \code{cpr_cnr} object
#' @param type type of diagnostic plot.
#' \code{"loglik"} for the log likelihood by degrees of freedom,
#' \code{"rmse"} for root mean squared residuals by model index
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... ignored
plot.cpr_cnr <- function(x, type = "rmse", from = 1, to, ...) {

  if (from < 1) {
    from <- 1
  }

  if (any(type %in% c("rmse", "loglik"))) {
    if (missing(to)) {
      to <- length(x)
    } else if (to > length(x)) {
      to <- length(x)
    }

    s <- summary(x)

    ggplot2::ggplot(subset(s, (s$index >= from) & (s$index <= to))) +
    ggplot2::theme_bw() +
    ggplot2::aes_string(x = "index", y = type) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Index")

  } else {
    stop("type needs to be either 'loglik' or 'rmse'.")
  }
}

