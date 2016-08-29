#' Control Net Reduction
#'
#' Run the Control Net Reduction Algorithm.
#'
#' \code{cnr} runs the control net reduction algorithm.  
#'
#' \code{keep} will keep the regression fit as part of the \code{cnr\_cp} object
#' for models with upto and including keep fits.  For example, if \code{keep =
#' 10} then the resulting \code{cnr\_cnr} object will have the regression fit
#' stored in the first \code{keep + 1} (zero internal knots, one internal knot,
#' \ldots, \code{keep} internal knots) \code{cnr\_cp} objects in the list.  The
#' limit on the number of stored regression fits is to keep memory usage down.
#'
#' @param x a \code{cnr_cp} or \code{cnr_tensor} object
#' @param p defaults to 2L, the L^p norm used in determining the influence
#'        weight of each internal knot.
#' @param progress show a progress bar.
#' @param ... not currently used
#' 
#' @export
cnr <- function(x, p = 2, progress = interactive(), ...) { 
  UseMethod("cnr")
}

#' @export
cnr.cpr_cn <- function(x, p = 2, progress = interactive(), ...) { 


  out <- vector("list", length = sum(sapply(lapply(x$bspline_list, attr, which = "iknots"), length)) + 1L)

  if (progress) { 
    pb <- utils::txtProgressBar(max = length(out), style = 3)
    prg <- 0
    utils::setTxtProgressBar(pb, prg)
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x 
    w <- cpr::influence_weights(x, p = p) 
    w <- dplyr::bind_rows(w, .id = "margin") 
    w <- dplyr::filter_(w, ~ `max(w)` > min(`max(w)`))

    nkts <- lapply(split(w, factor(w$margin, levels = seq_along(x$bspline_list))), function(xx) xx$iknots)

    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), evaluate = FALSE), parent.frame(), environment(x))

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

#' @method print cnr_cnr
#' @export
print.cnr_cnr <- function(x, ...) { 
  cat("A list of control nets\n")
  utils::str(x, max.level = 0)
}

