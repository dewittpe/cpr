#' Defunct Functions
#'
#' A major refactor of the package between v0.3.0 and v.0.4.0 took place and
#' many functions were made defunct.  The refactor was so extensive that moving
#' the functions to deprecated was not a viable option.
#'
#' @param ... pass through
#'
#' @name cpr-defunct
NULL

#' @rdname cpr-defunct
#' @export
refine_ordinate <- function(...) {
  .Defunct(new = "insert_a_knot")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname cpr-defunct
#' @export
coarsen_ordinate <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname cpr-defunct
#' @export
hat_ordinate <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname cpr-defunct
#' @export
insertion_matrix <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname cpr-defunct
#' @export
wiegh_iknots <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname cpr-defunct
#' @export
influence_of <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this S3 method in R/influence_of.R
}

#' @rdname cpr-defunct
#' @export
influence_weights <- function(...) {
  .Defunct("influence_of_iknots")
  # v0.3.0 defined this S3 method in R/influence_weights.R
}


# NONE EXPORTED DEFUNCT
extract_cpr_bsplines <- function(form) {
  .Defunct()
  # was part of the cn function from v0.3.0 and no longer needed
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
