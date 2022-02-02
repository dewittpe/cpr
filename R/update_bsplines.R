#' Update bspline or btensor calls
#'
#' Update \code{cpr_bs} and \code{cpr_bt} objects alone or within \code{cpr_cp}
#' and \code{cpr_cn} objects.
#'
#' @param object an object to update.  
#' @param ... things to update, expected to be \code{iknots}, \code{df},
#' \code{bknots}, or \code{order}.
#' @param evaluate If true evaluate the new call else return the call.
#'
#' @seealso \code{\link[stats]{update}}, \code{\link{bsplines}},
#' \code{\link{btensor}}
#'
#' @example examples/update_bsplines.R
#'
#' @export
#' @rdname update_bsplines
update_bsplines <- function(object, ..., evaluate = TRUE) {
  UseMethod("update_bsplines")
}

#' @export
update_bsplines.formula <- function(object, ..., evaluate = TRUE) {
  dots <- as.list(match.call(expand.dots = FALSE))$...
  dots <- dots[!is.na(match(names(dots), c("iknots", "df", "bknots", "order")))] 
  out <- lapply(as.list(object), find_update_b_, dots)
  out <- eval(as.call(out))
  environment(out) <- environment(object)
  out 
}

#' @export
update_bsplines.cpr_bs <- function(object, ..., evaluate = TRUE) { 
  cl <- as.list(attr(object, "call"))
  dots <- match.call(expand.dots = FALSE)$...
  dots <- dots[!is.na(match(names(dots), c("iknots", "df", "bknots", "order")))] 
  
  for(a in names(dots)) { 
    if (!all(c(is.null(cl[[a]]), is.null(dots[[a]])))) {
      cl[[a]] <- dots[[a]]
    }
  }

  if (evaluate) {
    eval(as.call(cl), attr(object, "environment"))
  } else {
    cl
  }
  
}

#' @export
update_bsplines.cpr_cp <- function(object, ..., evaluate = TRUE) {
  dots <- match.call(expand.dots = FALSE)$...
  f <- do.call(update_bsplines.formula, c(list(object = object$call$formula), dots)) 
  x <- stats::update(object, formula = f, evaluate = FALSE) 
  if (evaluate) {
    eval(x, environment(f), parent.frame())
  } else {
    x
  }
}

#' @export
#' @rdname update_bsplines
update_btensor <- function(object, ..., evaluate = TRUE) {
  UseMethod("update_btensor")
}

#' @export
update_btensor.formula <- update_bsplines.formula
#' @export
update_btensor.cpr_cn <- update_bsplines.cpr_cp
#' @export
update_btensor.cpr_bt <- update_bsplines.cpr_bs


find_update_b_ <- function(x, dots) {
  if (is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
    for(a in names(dots)) { 
      if (!all(c(is.null(x[[a]]), is.null(dots[[a]])))) {
        x[[a]] <- dots[[a]]
      }
    }
    x
  } else if (is.recursive(x)) {
    as.call(lapply(as.list(x), find_update_b_, dots))
  } else {
    x
  }
}


# newknots are used in the cpr and cnr calls.  No other use for this function.
# It should, at some point, be deprecated in favor of update_bsplines
newknots <- function(form, nk) { 
  rr <- function(x, nk) {
    if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      x$df <- NULL
      x$iknots <- nk
      x
    } else if (is.recursive(x)) {
      as.call(lapply(as.list(x), rr, nk))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr, nk)   
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
}
