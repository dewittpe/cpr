#' Control Net Reduction
#'
#' Run the Control Net Reduction Algorithm.
#'
#' \code{cnr} runs the control net reduction algorithm.
#'
#' \code{keep} will keep the regression fit as part of the \code{cnr\_cp} object
#' for models with up to and including keep fits.  For example, if \code{keep =
#' 10} then the resulting \code{cnr\_cnr} object will have the regression fit
#' stored in the first \code{keep + 1} (zero internal knots, one internal knot,
#' \ldots, \code{keep} internal knots) \code{cnr\_cp} objects in the list.  The
#' limit on the number of stored regression fits is to keep memory usage down.
#'
#' @param x a \code{cnr_cn} object
#' @param margin the margins to apply the CNR algorithm to.  Passed to
#' \code{\link{influence_weights}}.
#' @param n_polycoef the number of polynomial coefficients to use when assessing
#' the influence of each internal knot.
#' @param progress show a progress bar.
#' @param ... not currently used
#'
#' @return
#'
#' @seealso \code{\link{influence_weights}}, \code{\link{cpr}} for the
#' uni-variable version, Control Polygon Reduction.
#'
#' \code{vignette(topic = "cnr", package = "cpr")}
#'
#' @examples
#'
#' @export
cnr <- function(x, margin, n_polycoef = 20L, progress = interactive(), ...) {
  UseMethod("cnr")
}

#' @export
cnr.cpr_cn <- function(x, margin = seq_along(x$bspline_list), n_polycoef = 20L, progress = interactive(), ...) {

  out <- vector("list", length = sum(sapply(lapply(x$bspline_list[margin], attr, which = "iknots"), length)) + 1L)

  if (progress) {
    pb <- utils::txtProgressBar(max = length(out), style = 3) # nocov
    prg <- 0 # nocov
    utils::setTxtProgressBar(pb, prg) # nocov
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- summary(influence_of_iknots(out[[i]], margin, n_polycoef))
    w <- w[w$influence_rank > 1, ]
    nkts <- lapply(split(w, f = w$margin), getElement, "iknot")

    for ( margin_not_in_nkts in as.character(margin)[ !(as.character(margin) %in% names(nkts)) ] ) {
      nkts <- c(nkts, stats::setNames(list(numeric(0)), margin_not_in_nkts))
      nkts <- nkts[sort(names(nkts))]
    }

    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), keep_fit = TRUE, check_rank = FALSE, evaluate = FALSE), parent.frame())

    if (progress) {
      utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    }
  }

  out[[1]] <- x

  if (progress) {
    utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    close(pb) # nocov
  }

  class(out) <- c("cpr_cnr", class(out))
  out
}

#' @method print cpr_cnr
#' @export
print.cpr_cnr <- function(x, ...) {
  cat("A list of control nets\n")
  cat(utils::str(x, max.level = 0))
  invisible(x)
}
