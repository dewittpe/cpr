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
#' @param x data
#' @param iknots internal knots
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials
#'
#' @export
bsplines <- function(x, iknots = numeric(0), bknots = range(x), order = 4L) { 
  B <- .Call('cpr_bsplines__impl', PACKAGE = 'cpr', x, iknots, bknots, order) 
  out <- B$Bmat
  attr(out, "x")       <- x
  attr(out, "order")   <- B$order
  attr(out, "iknots")  <- B$iknots
  attr(out, "bknots")  <- B$bknots
  attr(out, "xi")      <- B$xi
  attr(out, "xi_star") <- B$xi_star
  attr(out, "call")    <- match.call()
  attr(out, "class")   <- c("cpr_bs", "bs", "basis", "matrix")
  out
}

#' @method print cpr_bs
#' @export
#' @rdname bsplines
#' @param n, number of rows of the B-spline basis matrix to display, defaults to
#' 6L.
#' @param ... not currently used
print.cpr_bs <- function(x, n = 6L, ...) { 
  print(attr(x, "call"))
  cat("Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  print(x[seq(1, min(nrow(x), n), by = 1L), ])
}

#' @method plot cpr_bs
#' @export
#' @rdname bsplines
plot.cpr_bs <- function(x, ...) {
  .data <- tidyr::gather_(cbind(as.data.frame(x), "x" = attr(x, "x")), 
                          key_col = "spline", 
                          value_col = "value", 
                          gather_cols = paste0("V", seq(1, ncol(x), by = 1L)))

  lgnd <- sapply(1:dplyr::n_distinct(.data$spline), function(b) { 
                 as.expression(paste0("italic(B)[list(", b, ", ", attr(x, "order"), ", bolditalic(xi))]")) })
  lgnd <- scales::parse_format()(lgnd)

  .data <- dplyr::tbl_df(.data)

  xi <- attr(x, "xi")
  k  <- attr(x, "order")

  xilb <- paste0("xi[", seq_along(xi), "] == ", "xi", "")
  xilb <- c(paste(head(xilb, k), collapse = "\n"),
            xilb[!(xi %in% range(xi))],
            paste(tail(xilb, k), collapse = "\n")) 
  xilb <- gsub("\\d\\n", "", xilb)


  expr <- list(bquote(group('{', xi[j], '}')[j == 1]^{.(k)} ))

  if (length(xi) > 2 * k) { 
    for(i in seq(k + 1, length(xi) - k, by = 1)) { 
      expr <- c(expr, bquote(xi[.(i)]))
    }
  }

  expr <- c(expr, bquote(group('{', xi[j], '}')[j == .(length(xi) - k)]^{.(length(xi))}))

  ggplot2::ggplot(.data) + 
  ggplot2::theme_bw() + 
  ggplot2::aes_string(x = "x", y = "value", color = "spline") + 
  ggplot2::geom_line() + 
  ggplot2::theme(axis.title = ggplot2::element_blank()) +
  ggplot2::scale_color_hue(name = "B-spline", labels = lgnd) + 
  ggplot2::scale_x_continuous(breaks = unique(xi), labels = do.call(expression, expr)) 
}

#' @export
#' @param xi full knot vector, 
#' @rdname bsplines
greville_sites <- function(xi, order = 4L) {
    .Call('cpr_greville_sites__impl', PACKAGE = 'cpr', xi, order)
}
