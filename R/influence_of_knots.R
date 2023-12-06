#' Determine the influence of the knots within a control polygon
#'
#' @param x \code{cpr_cp} object
#' @param ... pass through
#'
#' @examples
#' x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 5000)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' cp0 <- cp(bmat, theta)
#'
#' icp0 <- influence_of_knots(cp0)
#'
#' plot(cp0, icp0$coarsened_cps[[1]], icp0$restored_cps[[1]], color = TRUE, show_spline = TRUE)
#' plot(cp0, icp0$restored_cps[[1]], color = TRUE, show_spline = TRUE)
#'
#' plot(cp0, icp0$coarsened_cps[[2]], icp0$restored_cps[[2]], color = TRUE, show_spline = TRUE)
#' plot(cp0, icp0$restored_cps[[2]], color = TRUE, show_spline = TRUE)
#'
#' plot(cp0, icp0$coarsened_cps[[3]], icp0$restored_cps[[3]], color = TRUE, show_spline = TRUE)
#' plot(cp0, icp0$restored_cps[[3]], color = TRUE, show_spline = TRUE)
#'
#' plot(cp0, icp0$coarsened_cps[[4]], icp0$restored_cps[[4]], color = TRUE, show_spline = TRUE)
#' plot(cp0, icp0$restored_cps[[4]], color = TRUE, show_spline = TRUE)
#'
#' plot(cp0, icp0$coarsened_cps[[5]], icp0$restored_cps[[5]], color = TRUE, show_spline = TRUE)
#' plot(cp0, icp0$restored_cps[[5]], color = TRUE, show_spline = TRUE)
#'
#' (theta - icp0$restored_cps[[4]]$cp$theta)**2 |> sum()
#'
#'
#' @export
influence_of_knots <- function(x, ...) {
  UseMethod("influence_of_knots")
}

#' @export
influence_of_knots.cpr_cp <- function(x, ...) {

  # only work on the internal knots
  coarsened_thetas <-
    lapply(X = seq(x$order, x$order + length(x$iknots) - 1), 
           FUN = coarsen_theta, 
           xi = x$xi, 
           k = x$order, 
           theta = x$cp$theta)

  # just need the meta data for basis matrices
  coarsened_bmats <-
    lapply(X = seq_along(x$iknots),
           FUN = 
             function(j) {
               bsplines(numeric(0), iknots = x$iknots[-j], bknots = x$bknots, order = x$order)
             }
    )

  coarsened_cps <- Map(cp, x = coarsened_bmats, theta = coarsened_thetas)

  bmat0 <- bsplines(numeric(0), iknots = x$iknots, bknots = x$bknots, order = x$order)

  hat_thetas <- lapply(X = seq(x$order, x$order + length(x$iknots) - 1),
                       FUN = hat_theta,
                       xi = x$xi,
                       k = x$order,
                       theta = x$cp$theta)

  restored_cps <- mapply(function(x, hat_theta) {cp(x, hat_theta$theta)}, hat_theta = hat_thetas, MoreArgs = list(x = bmat0), SIMPLIFY = FALSE)

  rtn <- list(
              coarsened_cps = coarsened_cps,
              restored_cps  = restored_cps,
              d             = lapply(hat_thetas, getElement, "d"),
              influence     = sapply(hat_thetas, getElement, "influence")
              )

  rtn
}
