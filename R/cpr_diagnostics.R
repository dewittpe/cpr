#' Control Polygon Reduction Diagnoses
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#' 
#' @export
#' @rdname cpr_diagnostics
#' @param x a cpr_cpr object
#' @param tol max difference/error between the vertices of cp1 to cp2
cpr_select <- function(x, tol = 3) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x1, x2) }, 
                  x1 = x[-length(x)],
                  x2 = x[-1])
  awt <- lapply(x, function(x) { unname(attr(x, "weights"))})[-1]
  mwt <- do.call(c, lapply(awt, min))
  awt <- do.call(c, awt)

  dat <- 
    dplyr::data_frame(n_iknots      = seq(0, length(x) - 1L, by = 1L), 
                      min_weight    = c(NA, mwt), 
                      diffs         = c(diffs, NA)) 
  best_cp_index <- max(which(mwt > stats::median(awt) + tol * stats::IQR(awt))) + 1L

  out <- list(best_cp = x[[best_cp_index]], all_cps = dat)
  attr(out, "best_cp_index") <- best_cp_index 
  attr(out, "tol") <- tol
  class(out) <- c("cpr_selected", class(out))
  return(out)
}

#' @method print cpr_selected
#' @rdname cpr_diagnostics
#' @export
print.cpr_selected <- function(x, ...) { 
  cat("Selected model with", length(attr(x$best_cp, "iknots")), "internal knots:\n")
  print(attr(x$best_cp, "iknots")) 
  cat("\nThe selected CP is:\n")
  print(x$best_cp)
}

#' @method plot cpr_cpr
#' @rdname cpr_diagnostics
#' @export 
#' @param type type of diagnostic plot.  \code{"cp"} for control polygons,
#' \code{"ssr"} for sums of squared residuals by number of internal knots.
#' @param from the minimum number of interior knots to display
#' @param to the maximum number of interior knots to display
#' @param show_spline (boolean, default \code{FALSE}) if \code{TRUE} show the
#' spline function within the control polygons.  Ignored for \code{type = "ssr"}
#' @param color (boolean) use color in the plot(s)
#' @param n number of points to interpolate the spline function with.  Ignored
#' for \code{type = "ssr"}.
#' @param ... not currently used.
plot.cpr_cpr <- function(x, type = "cp", from = 0, to = min(9, length(x) - 1L), show_spline = FALSE, color = FALSE, n = 100, ...) { 
  if (type == "cp") { 
    lp <- suppressMessages(
                           lapply(seq(from, to - 1L, by = 1L) + 1L,
                                  function(idx, .data, sp, n) { 
                                    plot.cpr_cp(.data[[idx]], .data[[idx + 1L]], show_spline = sp, color = color, n = n) + 
                                    ggplot2::scale_linetype(name = "", labels = paste(idx + -1:0, "iknots")) + 
                                    ggplot2::scale_color_hue(name = "", labels = paste(idx + -1:0, "iknots")) + 
                                    ggplot2::theme(legend.position = "top",
                                                   axis.title = ggplot2::element_blank())
                                  },
                                  .data = x, 
                                  sp = show_spline, 
                                  n = n)) 
    lp <- lapply(lp, ggplot2::ggplotGrob) 
    lp <- gridExtra::grid.arrange(grobs = lp) 
  } else if (type == "ssr") { 
    lp <- suppressMessages(
                           lapply(seq(from, to - 1L, by = 1L) + 1L,
                                  function(idx, .data) { 
                                    data.frame(iknots = idx - 1L, ssr = attr(.data[[idx]], "ssr"))
                                  },
                                  .data = x)) 
    lp <- do.call(rbind, lp)

    lp <- ggplot2::ggplot(lp) + 
      ggplot2::aes_string(x = "iknots", y = "ssr") + 
      ggplot2::geom_line() + 
      ggplot2::xlab("Interior Knots") + 
      ggplot2::ylab("Sum of Squared Residuals") + 
      ggplot2::scale_x_continuous(breaks = seq(from, to, by = 1), #floor(sqrt(to - from))), 
                                  labels = seq(from, to, by = 1)) + #floor(sqrt(to - from)))) +
      ggplot2::theme_bw()

  } else { 
    stop("type needs to be either 'cp' or 'ssr'.")
  }
  invisible(lp)
}

