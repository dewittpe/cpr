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
#' @param progress controls the level of progress messaging.
#' @param ... not currently used
#'
#' @return A \code{cpr_cnr} object.  This is a list of \code{cpr_cn} objects.
#'
#' @seealso \code{\link{cn}} for defining a control net,
#' \code{\link{influence_weights}} for finding the influence of the internal
#' knots, \code{\link{cpr}} for the univariate version, Control Polygon
#' Reduction.
#'
#' \code{vignette(topic = "cnr", package = "cpr")}
#'
#' @examples
#'
#' acn <- cn(log10(pdg) ~ btensor(list(day, age)
#'                                , df = list(10, 8)
#'                                , bknots = list(c(-1, 1), c(44, 53)))
#'          , data = spdg)
#' cnr0 <- cnr(acn)
#' cnr0
#' summary(cnr0)
#' plot(cnr0)
#'
#' @export
cnr <- function(x, margin, n_polycoef = 20L, progress = c('cnr', 'influence', 'none'), ...) {
  UseMethod("cnr")
}

#' @export
cnr.cpr_cn <- function(x, margin = seq_along(x$bspline_list), n_polycoef = 20L, progress = c('cnr', 'influence', 'none'), ...) {

  progress <- match.arg(progress, several.ok = FALSE)

  out <- vector("list", length = sum(sapply(lapply(x$bspline_list[margin], attr, which = "iknots"), length)) + 1L)

  if (progress == 'cnr') {
    pb <- utils::txtProgressBar(max = length(out), style = 3) # nocov
    prg <- 0 # nocov
    utils::setTxtProgressBar(pb, prg) # nocov
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- summary(influence_of_iknots(out[[i]], margin = margin, n_polycoef = n_polycoef, verbose = (progress == 'influence'), ...))
    w <- w[w$influence_rank > 1, ]
    nkts <- lapply(split(w, f = w$margin), getElement, "iknot")

    for ( margin_not_in_nkts in as.character(margin)[ !(as.character(margin) %in% names(nkts)) ] ) {
      nkts <- c(nkts, stats::setNames(list(numeric(0)), margin_not_in_nkts))
      nkts <- nkts[sort(names(nkts))]
    }

    x <-
      eval(
        stats::update(
             x
           , formula = newknots(x$call$formula, nkts)
           , keep_fit = TRUE
           , check_rank = FALSE
           , evaluate = FALSE)
        , parent.frame()
      )

    if (progress == 'cnr') {
      utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    }
  }

  out[[1]] <- x

  if (progress == 'cnr') {
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
