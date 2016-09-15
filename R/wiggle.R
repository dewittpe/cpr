#' Wiggliness of a Spline function
#'
#' Calculate the integral of the squared second derivate of the spline function.
#'
#' \deqn{\int \left( \frac{\mathrm{d}^2}{\mathrm{d}x^2} f \left(x \right)
#' \right)^2 \mathrm{d}x.}{ \int (d^2 / dx^2 f(x))^2 dx}
#'
#' @param object a \code{cpr_cp} object
#' @param lower the lower limit of the intergral
#' @param upper the upper limit of the intergral
#' @param ... arguments passed to \code{stats::integrate}
#'
#' @seealso \code{\link{cp}} \code{\link[stats]{integrate}}
#'

#' @export
wiggle <- function(object, lower, upper, ...) { 
  UseMethod("wiggle")
}

#' @export
wiggle.cpr_cp <- function(object, lower = min(object$bknots), upper = max(object$bknots), ...) { 
  f <- function(x) { 
    (bsplineD(x, 
              iknots = object$iknots, 
              bknots = object$bknots,
              order  = object$order, 
              derivative = 2L) %*%
     matrix(object$cp$theta, ncol = 1))^2
  }

  stats::integrate(f, lower = lower, upper = upper, ...)
}
