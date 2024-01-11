#' Control Net Reduction Plots
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#'
#' @param x a \code{cpr_cnr} object
#' @param type type of diagnostic plot.
#' \code{"loglik"} for the log likelihood by degrees of freedom,
#' \code{"rse"} for residual standard error by model index
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... pass through
#'
#' @return a ggplot
#'
#' @examples
#' initial_cn <- cn(log10(pdg) ~ btensor(list(day, age)
#'                         , df = list(10, 8)
#'                         , bknots = list(c(-1, 1), c(44, 53))
#'                         )
#'           , data = spdg)
#'
#' cnr0 <- cnr(initial_cn)
#'
#' plot(cnr0)
#'
#' @method plot cpr_cnr
#' @export
plot.cpr_cnr <- function(x, type = "rse", from = 1, to, ...) {

  if (from < 1) {
    from <- 1
  }

  if (any(type %in% c("rse", "loglik"))) {
    if (missing(to)) {
      to <- length(x)
    } else if (to > length(x)) {
      to <- length(x)
    }

    s <- summary(x)

    ggplot2::ggplot(subset(s, (s$index >= from) & (s$index <= to))) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("index"), Y = as.name(type)))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Index")

  } else {
    stop("type needs to be either 'loglik' or 'rse'.")
  }
}
