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
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#;
#' @param x a \code{cnr_cp} or \code{cnr_tensor} object
#' @param keep keep (store) the regression fit for the first \code{keep}
#' \code{cpr_cn} objects in the list returned by \code{cnr}.
#' @param p defaults to 2L, the L^p norm used in determining the influence
#'        weight of each internal knot.  Passed to
#'        \code{\link{influence_weights}}.
#' @param margin the margins to apply the CNR algorithm to.  Passed to
#' \code{\link{influence_weights}}.
#' @param n_polycoef the number of polynomial coefficients to use when assessing
#' the influence of each internal knot.
#' @param progress show a progress bar.
#' @param ... not currently used
#'
#' @seealso \code{\link{influence_weights}}, \code{\link{cpr}} for the
#' uni-variable version, Control Polygon Reduction.
#'
#' @export
cnr <- function(x, keep = -1, p = 2, margin, n_polycoef = 20L, progress = interactive(), ...) {
  UseMethod("cnr")
}

#' @export
cnr.cpr_cn <- function(x, keep = -1, p = 2, margin = seq_along(x$bspline_list), n_polycoef = 20L, progress = interactive(), ...) {

  out <- vector("list", length = sum(sapply(lapply(x$bspline_list[margin], attr, which = "iknots"), length)) + 1L)

  if (length(out) > (keep + 1) & x$keep_fit) {
    x <- eval(stats::update(x, keep_fit = FALSE, check_rank = FALSE, evaluate = FALSE), parent.frame())
  } else if (length(out) <= (keep + 1) & !x$keep_fit) {
    x <- eval(stats::update(x, keep_fit = TRUE, check_rank = FALSE, evaluate = FALSE), parent.frame())
  }

  if (progress) {
    pb <- utils::txtProgressBar(max = length(out), style = 3)
    prg <- 0
    utils::setTxtProgressBar(pb, prg)
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- influence_weights(x, p = p, margin, n_polycoef)
    for(i in seq_along(w)) {
      w[[i]]$margin <- i
    }
    print("=======================")
    print("i")
    print(i)
    print(w)
    w <- do.call(rbind, w)

    print(w)
    w <- subset(w, rank(w[["max_w"]], ties.method = "first") > 1)
    print(w)

    nkts <- lapply(split(w, factor(w$margin, levels = seq_along(x$bspline_list))), function(xx) xx$iknots)

    if (i == keep + 1) {
      x <- stats::update(x, keep_fit = TRUE)
    }

    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), check_rank = FALSE, evaluate = FALSE), parent.frame())

    if (progress) {
      utils::setTxtProgressBar(pb, prg <- prg + 1)
    }
  }

  out[[1]] <- x

  if (progress) {
    utils::setTxtProgressBar(pb, prg <- prg + 1)
    close(pb)
  }

  class(out) <- c("cpr_cnr", class(out))
  out
}

#' @method print cpr_cnr
#' @export
print.cpr_cnr <- function(x, ...) {
  cat("A list of control nets\n")
  utils::str(x, max.level = 0)
}

#' @export
#' @param object a \code{cpr_cnr} object
#' @rdname cnr
summary.cpr_cnr <- function(object, ...) {
  rtn <- lapply(object, summary)
  for (i in seq_along(rtn)) {
    rtn[[i]]$index <- as.integer(i)
  }
  do.call(rbind, rtn)
}

