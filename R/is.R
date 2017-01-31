#' Tests for Objects
#' 
#' Test if an R object is of a class specific to the cpr package
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x an R object
#'
#' @export
#' @rdname is
is.cpr_bs <- function(x) {
  inherits(x, "cpr_bs")
}

#' @export
#' @rdname is
is.cpr_bt <- function(x) {
  inherits(x, "cpr_bt")
}

#' @export
#' @rdname is
is.cpr_cp <- function(x) {
  inherits(x, "cpr_cp")
}

#' @export
#' @rdname is
is.cpr_cpr <- function(x) {
  inherits(x, "cpr_cpr")
}

#' @export
#' @rdname is
is.cpr_cn <- function(x) {
  inherits(x, "cpr_cn")
}

#' @export
#' @rdname is
is.cpr_cnr <- function(x) {
  inherits(x, "cpr_cnr")
}


