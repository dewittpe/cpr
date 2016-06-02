#' B-Splines
#'
#' An implimentation of Carl de Boor's recursive algorimthm for building
#' B-splines.
#'
#' The difference between this function and \code{splines::bs} come in the
#' attributes associated with the output and default options.  The
#' \code{cpr::bs} call is intended to simplify the work needed with respect to
#' the control polygon reduction.
#'
#' \code{greville_sites} returns the knot averages.
#'
#' @param x a numeric vector
#' @param iknots internal knots
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials, defualts to 4L.
#'
#' @examples
#' # build a vector of values to transform
#' xvec <- seq(-3, 5, length = 100)
#' 
#' # cubic b-spline
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 0.2))
#' bmat
#' 
#' # view the splints
#' plot(bmat)
#' 
#' # If you want a second x-axis to show the x-values try the following:
#' second_x_axis <- round(stats::quantile(xvec, probs = seq(0, 1, by = .2)), 2)
#' 
#' plot(bmat) + 
#' ggplot2::annotate(geom = "text", x = second_x_axis, y = -0.02, label = second_x_axis) + 
#' ggplot2::annotate(geom = "linerange", x = second_x_axis, ymin = -0.05, ymax = -0.04) + 
#' ggplot2::coord_cartesian(ylim = c(0, 1))
#' 
#' # quadratic splines
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 0.2), order = 3L)
#' bmat
#' plot(bmat) + ggplot2::ggtitle("Quadratic B-splines")
#' 
#' @export
bsplines <- function(x, iknots = numeric(0), bknots = range(x), order = 4L) { 
  B <- .Call('cpr_bsplines__impl', PACKAGE = 'cpr', x, iknots, bknots, order) 
  out <- B$Bmat
  # attr(out, "x")       <- x
  # attr(out, "call")    <- match.call()
  attr(out, "iknots")  <- B$iknots
  attr(out, "bknots")  <- B$bknots
  attr(out, "xi")      <- B$xi
  attr(out, "xi_star") <- B$xi_star
  attr(out, "order")   <- B$order
  attr(out, "class")   <- c("cpr_bs", "bs", "basis", "matrix")
  out
}

#' Print bsplines
#'
#' @method print cpr_bs
#' @export
#' @param x a \code{cpr_bs} object.
#' @param n, number of rows of the B-spline basis matrix to display, defaults to
#' 6L.
#' @param \ldots not currently used.
print.cpr_bs <- function(x, n = 6L, ...) { 
  # print(attr(x, "call"))
  cat("Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  print(x[seq(1, min(nrow(x), abs(n)), by = 1L), ])
  # dplyr:::print.tbl_df(dplyr::tbl_df(as.data.frame(x)))
}

#' Plot B-splines
#'
#' @method plot cpr_bs
#' @export
#' @param x a \code{cpr_bs} object
#' @param n number of values to use to plot the splines, defaults to 100
#' @param \ldots not currently used
plot.cpr_bs <- function(x, ..., n = 100) {
  xvec <- seq(min(attr(x, "bknots")), max(attr(x, "bknots")), length = n)
  bmat <- bsplines(xvec, iknots = attr(x, "iknots"), order = attr(x, "order"))
  .data <- tidyr::gather_(cbind(as.data.frame(bmat), "x" = xvec),
                          key_col = "spline", 
                          value_col = "value", 
                          gather_cols = paste0("V", seq(1, ncol(x), by = 1L)))

  lgnd <- sapply(1:dplyr::n_distinct(.data$spline), function(b) { 
                 as.expression(paste0("italic(B)[list(", b, ", ", attr(x, "order"), ", bolditalic(xi))]")) })
  lgnd <- scales::parse_format()(lgnd)

  .data <- dplyr::tbl_df(.data)
  # return(.data)

  xi <- attr(x, "xi")
  k  <- attr(x, "order")

  xilb <- paste0("xi[", seq_along(xi), "] == ", "xi", "")
  xilb <- c(paste(utils::head(xilb, k), collapse = "\n"),
            xilb[!(xi %in% range(xi))],
            paste(utils::tail(xilb, k), collapse = "\n")) 
  xilb <- gsub("\\d\\n", "", xilb)


  expr <- list(bquote(group('{', xi[j], '}')[j == 1]^{.(k)} ))

  if (length(xi) > 2 * k) { 
    for(i in seq(k + 1, length(xi) - k, by = 1)) { 
      expr <- c(expr, bquote(xi[.(i)]))
    }
  }

  expr <- c(expr, bquote(group('{', xi[j], '}')[j == .(length(xi) - k + 1L)]^{.(length(xi))}))

  ggplot2::ggplot(.data) + 
  ggplot2::theme_bw() + 
  ggplot2::aes_string(x = "x", y = "value", color = "spline") + 
  ggplot2::geom_line() + 
  ggplot2::theme(axis.title = ggplot2::element_blank()) +
  ggplot2::scale_color_hue(name = "B-spline", labels = lgnd) + 
  ggplot2::scale_x_continuous(breaks = unique(xi), labels = do.call(expression, expr))
}

#' @export
#' @rdname bsplines
is.cpr_bs <- function(x) { 
  inherits(x, "cpr_bs")
}
