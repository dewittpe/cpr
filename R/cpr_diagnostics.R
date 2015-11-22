#' Control Polygon Reduction Diagnoses
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#'
#' 
#' @export
#' @rdname cpr_diagnostics
#' @param obj a cpr_cpr object
#' @param err max difference/error between the vertices of cp1 to cp2
cpr_select <- function(obj, err = 0.01) { 
  UseMethod("cpr_select")
}

cpr_select.cpr_cpr <- function(obj, err = 0.01) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x2, x1) }, 
                  x1 = obj[-length(obj)],
                  x2 = obj[-1])
  mwt <- sapply(obj, function(x) { unname(min(attr(x, "weights"))) })

  # min(which(!sapply(diffs, function(x) { all(x < err) })))
  dplyr::data_frame(n_iknots      = seq(0, length(obj) - 1L, by = 1), 
                    min_weight    = mwt, 
                    all_diffs_under_err = c(sapply(diffs, function(x) { all(x < err) }), NA),
                    diffs         = c(diffs, NA))
}

#' @export 
#' @rdname cpr_diagnostics
#' @param start index of the first control polygon to plot
#' @param end index of the last control polygon to plot
cpr_layers <- function(obj, start = 1L, end = 6L) { 
  dat <- lapply(seq(start, end, by = 1L), function(i) { dplyr::mutate(obj[[i]]$cp, index = as.character(i)) })
  dat <- do.call(rbind, dat)

  list(ggplot2::geom_point(data = dat, mapping = ggplot2::aes_string(x = "xi_star", y = "theta", color = "index")),
       ggplot2::geom_line(data = dat, mapping = ggplot2::aes_string(x = "xi_star", y = "theta", color = "index")))
}


