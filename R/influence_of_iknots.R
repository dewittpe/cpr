#' Determine the influence of the internal knots of a control polygon
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
#' @export
influence_of_iknots <- function(x, ...) {
  UseMethod("influence_of_iknots")
}

#' @export
influence_of_iknots.cpr_cp <- function(x, ...) {

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
              original_cp   = x,
              coarsened_cps = coarsened_cps,
              restored_cps  = restored_cps,
              d             = lapply(hat_thetas, getElement, "d"),
              influence     = sapply(hat_thetas, getElement, "influence")
              )

  class(rtn) <- "cpr_influence_of_iknots"

  rtn
}
#' @export
summary.cpr_influence_of_iknots <- function(x, ...) {
  if (length(x$original_cp$iknots) == 0L) {
    message("no internal knots")
    return(invisible(x))
  }

  data.frame(#iknot_idx = seq_along(x$influence)
             #iknot = paste0("xi_", seq(x$original_cp$order + 1, x$original_cp$order + length(x$original_cp$iknots))) ,
             iknot = x$original_cp$iknots,
             influence = x$influence,
             rank = rank(x$influence, ties.method = "first")
        )

}

#' @export
print.cpr_influence_of_iknots <- function(x, ...) {
  x$influence
}

#' @export
plot.cpr_influence_of_iknots <- function(x, j, coarsened = FALSE, restored = TRUE, ...) {
  if (length(x$original_cp$iknots) == 0L) {
    stop("no internal knots - nothing to plot")
  }

  if (missing(j)) {
    j <- seq_along(x$original_cp$iknots)
  } else {
    j <- as.integer(j)
    stopifnot(j >= 1L)
    stopifnot(j <= length(x$original_cp$iknots))
  }

  Original <- x$original_cp
  plots <- list()
  for(i in j) {
    Coarsened <- x$coarsened_cps[[i]]
    Restored  <- x$restored_cps[[i]]
    if (coarsened & restored) {
      plots <- c(plots, list(plot(Original, Coarsened, Restored, ...)))
    } else if (coarsened & !restored) {
      plots <- c(plots, list(plot(Original, Coarsened, ...)))
    } else if (!coarsened & restored) {
      plots <- c(plots, list(plot(Original, Restored, ...)))
    } else {
      plots <- c(plots, list(plot(Original, ...)))
    }
  }

  if (length(j) == 1) {
    plots[[1]]
  } else {
    plots
  }
}
