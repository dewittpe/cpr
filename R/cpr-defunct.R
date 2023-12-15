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
