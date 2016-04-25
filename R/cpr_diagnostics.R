#' Control Polygon Reduction Diagnoses
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#'
#' 
#' @export
#' @rdname cpr_diagnostics
#' @param obj a cpr_cpr object
#' @param tol max difference/error between the vertices of cp1 to cp2
cpr_select <- function(obj, tol = 3) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x1, x2) }, 
                  x1 = obj[-length(obj)],
                  x2 = obj[-1])
  awt <- lapply(obj, function(x) { unname(attr(x, "weights"))})[-1]
  mwt <- do.call(c, lapply(awt, min))
  awt <- do.call(c, awt)

  dat <- 
    dplyr::data_frame(n_iknots      = seq(0, length(obj) - 1L, by = 1L), 
                      min_weight    = c(NA, mwt), 
                      diffs         = c(diffs, NA)) 
  best_cp_index <- max(which(mwt > median(awt) + tol * IQR(awt))) + 1L

  out <- list(best_cp = obj[[best_cp_index]], all_cps = dat)
  attr(out, "best_cp_index") <- best_cp_index 
  attr(out, "tol") <- tol
  class(out) <- c("cpr_selected", class(out))
  return(out)
}

#' @method print cpr_selected
#' @export
print.cpr_selected <- function(x, ...) { 
  cat("Selected model with", length(attr(x$best_cp, "iknots")), "internal knots:\n")
  print(attr(x$best_cp, "iknots")) 
  cat("\nThe selected CP is:\n")
  print(x$best_cp)
}

#' @method plot cpr_cpr
#' @export 
plot.cpr_cpr <- function(x, from = 0, to = 9, show_spline = FALSE, n = 500, ...) { 
  lp <- suppressMessages(
                         lapply(seq(from, to - 1L, by = 1L) + 1L,
                                function(idx, .data, sp, n) { 
                                  plot.cpr_cp(.data[[idx]], .data[[idx + 1L]], show_spline = sp, n = n) + 
                                  ggplot2::scale_linetype(name = "", labels = paste(idx + -1:0, "iknots")) + 
                                  ggplot2::theme(legend.position = "top",
                                                 axis.title = ggplot2::element_blank())
                                },
                                .data = x, 
                                sp = show_spline, 
                                n = n)) 
  lp <- lapply(lp, ggplot2::ggplotGrob) 
  gridExtra::grid.arrange(grobs = lp) 
}

