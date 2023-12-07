#' Defunct Functions
#'
#' A major refactor of the package between v0.3.0 and v.0.4.0 took place and
#' many functions were made defunct.  The refactor was so extensive that moving
#' the functions to deprecated was not a viable option.
#'
#' @param ... pass through
#'
#' @name Defunct
NULL

#' @rdname Defunct
#' @export
refine_ordinate <- function(...) {
  .Defunct(new = "insert_a_knot")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname Defunct
#' @export
coarsen_ordinate <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname Defunct
#' @export
hat_ordinate <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname Defunct
#' @export
insertion_matrix <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname Defunct
#' @export
wiegh_iknots <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this in src/boehm.cpp
}

#' @rdname Defunct
#' @export
influence_of <- function(...) {
  .Defunct(new = "influence_of_iknots")
  # v0.3.0 defined this S3 method in R/influence_of.R
}

#' @rdname Defunct
#' @export
influence_weights <- function(...) {
  .Defunct("influence_of_iknots")
  # v0.3.0 defined this S3 method in R/influence_weights.R
}
