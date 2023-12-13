#' Control Polygon Diagnostics
#'
#' Collection of functions for inspection and analysis of the control polygons
#'
#' @return
#' \code{cp_value} returns the ordinate on the control polygon line segment for
#' the abscissa \code{x} given.  \code{x} could be a control vertex or on a
#' line segment defined by two control vertices of the control polygon
#' provided.
#'
#' \code{cp_diff} returns the absolute vertical distance between the control
#' vertices of cp1 to the control polygon cp2.
#'
#' @param x abscissa at which to determine the ordinate on control polygon cp
#' @param obj a cpr_cp object or \code{data.frame} where the first column is the
#' abscissa and the second column is the ordinate for the control polygon vertices.
#'
#' @examples
#'
#' @export
#' @rdname cp_diagnostics
cp_value <- function(obj, x) {
  UseMethod("cp_value")
}

#' @export
cp_value.cpr_cp <- function(obj, x) {
  xi_star <- obj$xi_star
  theta   <- obj$theta

  idx <- min(which(xi_star >= x)) + as.numeric(x == min(xi_star))

  unname((theta[idx] - theta[idx - 1L]) / (xi_star[idx] - xi_star[idx - 1L]) * (x - xi_star[idx]) + theta[idx])
}

#' @export
#' @rdname cp_diagnostics
#' @param cp1 a cpr_cp object
#' @param cp2 a cpr_cp object
cp_diff <- function(cp1, cp2) {
  UseMethod("cp_diff")
}

#' @export
cp_diff.cpr_cp <- function(cp1, cp2) {
  stopifnot(inherits(cp2, "cpr_cp"))
  unname(abs(sapply(cp1$xi_star, function(x) {cp_value(obj = cp2, x)}) - cp1$theta))
}
