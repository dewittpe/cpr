#' Plot B-spline Basis
#'
#' Wrapper around several ggplot2 calls to plot a B-spline basis
#'
#' @param x a \code{cpr_bs} object
#' @param show_xi logical, show the knot locations, using the Greek letter xi, on the x-axis
#' @param show_x  logical, show the x values of the knots on the x-axis
#' @param color logical, if \code{TRUE} (default) the splines are plotted in
#' color.  If \code{FALSE} all splines are black lines.
#' @param digits number of digits to the right of the decimal place to report
#' for the value of each knot.
#' @param n number of values to use to plot the splines, defaults to 100
#' @param \ldots not currently used
#'
#' @return a ggplot
#'
#' @seealso \code{\link{bsplines}}
#'
#' @examples
#' bmat <- bsplines(seq(-3, 2, length = 1000), iknots = c(-2, 0, 0.2))
#' plot(bmat, show_xi = TRUE,  show_x = TRUE)
#' plot(bmat, show_xi = FALSE, show_x = TRUE)
#' plot(bmat, show_xi = TRUE,  show_x = FALSE)  ## Default
#' plot(bmat, show_xi = FALSE, show_x = FALSE)
#' plot(bmat, show_xi = FALSE, show_x = FALSE)
#' plot(bmat, show_xi = FALSE, show_x = FALSE, color = FALSE)
#' @method plot cpr_bs
#' @export
plot.cpr_bs <- function(x, ..., show_xi = TRUE, show_x = FALSE, color = TRUE, digits = 2, n = 100) {

  # reshape from wide to long and from matrix to data.frame
  plot_data <- utils::stack(as.data.frame(x))
  names(plot_data) <- c("value", "spline")

  xvec <- seq(attr(x, "bknots")[1], attr(x, "bknots")[2], length = nrow(x))
  plot_data <- cbind(plot_data, data.frame(x = rep(xvec, times = ncol(x))))

  levels(plot_data$spline) <- sub("V", "B", levels(plot_data$spline))
  levels(plot_data$spline) <- sub("(\\d+)",
                                  paste0("[list(\\1,k==", attr(x, "order"), ",bold(xi))](x)"),
                                        levels(plot_data$spline))

  g <-
    ggplot2::ggplot(plot_data) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("x"), Y = as.name("value")))) +
    ggplot2::geom_line() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (color) {
    g <- g + eval(substitute(ggplot2::aes(color = GRP), list(GRP = as.name("spline"))))
    g <- g + ggplot2::scale_color_discrete(labels = scales::parse_format())
  } else {
    g <- g + eval(substitute(ggplot2::aes(group = GRP), list(GRP = as.name("spline"))))
  }

  if (show_xi | show_x) {
    e <- knot_expr(x, digits)

    if (show_xi & !show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = parse(text = e$xi_expr),
                                           minor_breaks = NULL)
    } else if (!show_xi & show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = e$num_expr,
                                           minor_breaks = NULL)
    } else {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = parse(text = e$xi_expr),
                                           minor_breaks = NULL,
                                           sec.axis = ggplot2::sec_axis(~ .,
                                                                        breaks = e$breaks,
                                                                        labels = e$num_expr))
    }

  }
  g
}
