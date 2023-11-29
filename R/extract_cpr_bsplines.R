#' Extract the bspline or btensor call from a formula
#'
#' Non-exported function.  Might be depreciated.
#'
#' @param form a formula
extract_cpr_bsplines <- function(form) { 
  B <- NULL
  rr <- function(x) { 
    if (is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) { 
      B <<- x
    } else if (is.recursive(x)) { 
      as.call(lapply(as.list(x), rr))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr)
  B
}
