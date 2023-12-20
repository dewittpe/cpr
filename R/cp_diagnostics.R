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
#' \code{cp_diff} returns the vertical distance between the control
#' vertices of cp1 to the control polygon cp2.
#'
#' @param obj a cpr_cp object or \code{data.frame} where the first column is the
#' abscissa and the second column is the ordinate for the control polygon vertices.
#' @param x abscissa at which to determine the ordinate on control polygon cp
#'
#' @seealso \code{\link{cp}}, \code{\link{cp_diff}}
#'
#' @examples
#' xvec <- seq(0, 6, length = 500)
#'
#' # Define the basis matrix
#' bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5))
#' bmat2 <- bsplines(x = xvec)
#'
#' # Define the control vertices ordinates
#' theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
#' theta2 <- c(1, 3.4, -2, 1.7)
#'
#' # build the two control polygons
#' cp1 <- cp(bmat1, theta1)
#' cp2 <- cp(bmat2, theta2)
#'
#' x <- c(0.2, 0.8, 1.3, 1.73, 2.15, 3.14, 4.22, 4.88, 5.3, 5.9)
#' cp_value(cp1, x = x)
#'
#' df <- data.frame(x = x, y = cp_value(cp1, x = x))
#'
#' plot(cp1, show_x = TRUE, show_spline = TRUE) +
#' ggplot2::geom_point(data = df
#'   , mapping = ggplot2::aes(x = x, y = y)
#'   , color = "red"
#'   , shape = 4
#'   , size = 3
#'   , inherit.aes = FALSE)
#'
#' @export
cp_value <- function(obj, x) {
  UseMethod("cp_value")
}

#' @export
cp_value.cpr_cp <- function(obj, x) {
  xi_star <- obj$cp$xi_star
  theta   <- obj$cp$theta

  idx <- sapply(x, function(x) {
                  min(which(xi_star >= x)) + as.numeric(x == min(xi_star))
  })

  unname((theta[idx] - theta[idx - 1L]) / (xi_star[idx] - xi_star[idx - 1L]) * (x - xi_star[idx]) + theta[idx])
}
