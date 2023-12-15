#' Wiggliness of a Spline function
#'
#' Calculate the integral of the squared second derivative of the spline
#' function.
#'
#' The wiggliness of the spline function is defined as
#'
#' \deqn{\int \left( \frac{\mathrm{d}^2}{\mathrm{d}x^2} f \left(x \right)
#' \right)^2 \mathrm{d}x.}{ \int (d^2 / dx^2 f(x))^2 dx}
#'
#' @param object a \code{cpr_cp} object
#' @param lower the lower limit of the integral
#' @param upper the upper limit of the integral
#' @param stop.on.error default to \code{FALSE}, see \code{\link[stats]{integrate}}.
#' @param ... additional arguments passed to \code{\link[stats]{integrate}}
#'
#' @return Same as \code{\link[stats]{integrate}}.
#'
#' @seealso \code{\link{cp}}, \code{\link[stats]{integrate}},
#' \code{\link{sign_changes}}
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
#' plot(cp1, cp2, show_cp = FALSE, show_spline = TRUE)
#'
#' wiggle(cp1)
#' wiggle(cp2)
#'
#' @export
wiggle <- function(object, lower, upper, stop.on.error = FALSE, ...) {
  UseMethod("wiggle")
}

#' @export
wiggle.cpr_cp <- function(object, lower = min(object$bknots), upper = max(object$bknots), stop.on.error = FALSE, ...) {
  f <- function(x) {
    (bsplineD(x,
              iknots = object$iknots,
              bknots = object$bknots,
              order  = object$order,
              derivative = 2L) %*%
     matrix(object$cp$theta, ncol = 1))^2
  }

  stats::integrate(f, lower = lower, upper = upper, stop.on.error = stop.on.error, ...)
}


#' Sign Changes
#'
#' Count the number of times the first, or second, derivative of a spline
#' changes sign.
#'
#' @param object a \code{cpr_cp} object
#' @param lower the lower limit of the integral
#' @param upper the upper limit of the integral
#' @param n number of values to assess the derivative between \code{lower} and
#' \code{upper}.
#' @param derivative integer value denoted first or second derivative
#' @param ... pass through
#'
#' @return the number of times the sign of the first or second derivative
#' changes within the specified interval.
#'
#' @seealso \code{\link{wiggle}}
#'
#' @examples
#'
#' @export
sign_changes <- function(object, lower = min(object$bknots), upper = max(object$bknots), n = 1000, derivative = 1L, ...) {
  UseMethod("sign_changes")
}

#' @export
sign_changes.cpr_cp <- function(object, lower = min(object$bknots), upper = max(object$bknots), n = 1000, derivative = 1L, ...) {

  f <- function(x) {
    bsplineD(x,
              iknots = object$iknots,
              bknots = object$bknots,
              order  = object$order,
              derivative = derivative) %*%
     matrix(object$cp$theta, ncol = 1)
  }

  if (upper >= max(object$bknots)) {
    upper <- max(object$bknots) - sqrt(.Machine$double.eps)
  }
  if (lower < min(object$bknots)) {
    lower <- min(object$bknots)
  }

  x <- seq(lower, upper, length.out = n)
  sum(abs(diff(sign(f(x))))) / 2
}
