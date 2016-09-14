#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.  
#'
#' \code{keep} will keep the regression fit as part of the \code{cpr\_cp} object
#' for models with upto and including keep fits.  For example, if \code{keep =
#' 10} then the resulting \code{cpr\_cpr} object will have the regression fit
#' stored in the first \code{keep + 1} (zero internal knots, one internal knot,
#' \ldots, \code{keep} internal knots) \code{cpr\_cp} objects in the list.  The
#' limit on the number of stored regression fits is to keep memory usage down.
#'
#' @param x a \code{cpr_cp} or \code{cpr_tensor} object
#' @param p defaults to 2L, the L^p norm used in determining the influence
#'        weight of each internal knot.
#' @param progress show a progress bar.
#' @param ... not currently used
#' 
#' @export
cpr <- function(x, p = 2, progress = interactive(), ...) { 
  UseMethod("cpr")
}

#' @export
cpr.cpr_cp <- function(x, p = 2, progress = interactive(), ...) { 

  out <- vector("list", length = length(x$iknots) + 1L)

  if (progress) { 
    pb <- utils::txtProgressBar(max = length(out), style = 3)
    prg <- 0
    utils::setTxtProgressBar(pb, prg)
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x 
    w    <- cpr::influence_weights(x, p = p) 
    nkts <- w$iknots[-which.min(w$w)] 
    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), evaluate = FALSE), parent.frame())

    if (progress) {
      utils::setTxtProgressBar(pb, prg <- prg + 1)
    }
  }

  out[[1]] <- x

  if (progress) {
    utils::setTxtProgressBar(pb, prg <- prg + 1)
    close(pb)
  } 

  class(out) <- c("cpr_cpr", class(out))
  out 
}

#' @method print cpr_cpr
#' @export
print.cpr_cpr <- function(x, ...) { 
  cat("A list of control polygons\n")
  utils::str(x, max.level = 0)
}

#' @export
#' @rdname cpr
is.cpr_cpr <- function(x) {
  inherits("cpr_cpr")
}

