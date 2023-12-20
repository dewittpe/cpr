#' Control Polygon Reduction Plots
#'
#' A wrapper around several ggplot2 calls to help evaluate results of a CPR run.
#'
#' @param x a \code{cpr_cpr} object
#' @param type type of diagnostic plot.  \code{"cps"} for control polygons,
#' \code{"loglik"} for the log likelihood by degrees of freedom,
#' \code{"rse"} for residual standard error by index.
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... arguments passed to \code{plot.cpr_cp}
#'
#' @seealso \code{\link{plot.cpr_cp}}, \code{\link{cpr}}, \code{\link{cp}}
#'
#' @return
#'
#' @examples
#'
#' @method plot cpr_cpr
#' @export
plot.cpr_cpr <- function(x, type = c("cps", "loglik", "rss", "rse"), from = 1, to, ...) {

  if (from < 1) {
    from <- 1
  }

  type <- match.arg(type, several.ok = FALSE)

  if (type == "cps") {

    if (missing(to)) {
      to <- 6
    } else if (to > length(x)) {
      to <- length(x)
    }

    nm <- deparse(substitute(x))

    eval(parse(text = paste("plot(",
                            paste(paste0(nm, "[[", seq(from = from, to = to, by = 1), "]]"),
                                  collapse = ", "),
                            ", ...)")))

  } else {
    if (missing(to)) {
      to <- length(x)
    } else if (to > length(x)) {
      to <- length(x)
    }

    s <- summary(x)
    s$index <- seq_along(s[[type]])

    ggplot2::ggplot(subset(s, (s$index >= from) & (s$index <= to))) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("index"), Y = as.name(type)))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Index")

  }
}
