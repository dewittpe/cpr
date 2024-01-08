#' Determine the influence of the internal knots of a control polygon
#'
#' @param x \code{cpr_cp} or \code{cpr_cn} object
#' @param verbose print status messages
#' @param cl interger passed to \code{\link[parallel]{mclapply}} and
#' \code{\link[parallel]{mcmapply}} other methods within the pbapply package.
#' Will be set to 1 when \code{.Platform$OS.type == "windows"} since windows
#' does not support forking.
#' @param ... pass through
#'
#' @return a \code{cpr_influence_of_iknots} object.  A list of six elements:
#' \describe{
#' \item{original_cp}{}
#' \item{coarsened_cps}{}
#' \item{restored_cps}{}
#' \item{d}{}
#' \item{influence}{}
#' \item{chisq}{}
#' }
#'
#' @examples
#' x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 5000)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' cp0 <- cp(bmat, theta)
#'
#' icp0 <- influence_of_iknots(cp0)
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
#' # When the cp was defined by regression
#' df <- data.frame(x = x, y = as.numeric(bmat %*% theta) + rnorm(5000, sd = 0.2))
#' cp1 <- cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4, 4.5), bknots = c(0, 6)), data = df)
#' icp1 <- influence_of_iknots(cp1)
#' icp1
#'
#' @export
influence_of_iknots <- function(x, verbose = FALSE, cl = 2L, calculate_test_statistic = TRUE, ...) {
  UseMethod("influence_of_iknots")
}

#' @export
influence_of_iknots.cpr_cp <- function(x, verbose = FALSE, cl = 2L, calculate_test_statistic = TRUE, ...) {

  if (.Platform$OS.type == "windows")  { # nocov
    warning("Windows does not support forking. cl being set to 1.") # nocov
    cl <- 1L # nocov
  } # nocov

  cl <- as.integer(cl)
  if (cl < 1) {
    cl <- 1L
  }

  if (length(x$iknots) > 0) {

    if (verbose) {
      LAPPLY <- function(X, FUN, cl = 2L, ...) pbapply::pblapply(X = X, FUN = FUN, ..., cl = cl)
      MAP <- function(FUN, cl = 2L, ...) pbapply::pbMap(f = FUN, ..., cl = cl)
    } else {
      LAPPLY <- function(X, FUN, cl = 2L, ...) parallel::mclapply(X = X, FUN = FUN, ..., mc.cores = cl)
      MAP <- function(FUN, cl = 2L, ...) parallel::mcmapply(FUN = FUN, ..., mc.cores = cl, SIMPLIFY = FALSE)
    }

    if (verbose) {
      message("\nThere are ", length(x$iknots), " internal knots to evaluate\n")
    }

    # only work on the internal knots
    if (verbose) {
      message("  Coarsening theta (step 1 of 6)")
    }

    coarsened_thetas <-
      LAPPLY(
          X     = seq(x$order, x$order + length(x$iknots) - 1)
        , FUN   = coarsen_theta
        , xi    = x$xi
        , k     = x$order
        , theta = x$cp$theta
        , cl    = 2
      )

    # just need the meta data for basis matrices
    if (verbose) {
      message("  getting meta data for coarsend basis matrices (step 2 of 6)")
    }

    coarsened_bmats <-
      LAPPLY(
          X = seq_along(x$iknots)
        , FUN =
               function(j) {
                 bsplines(numeric(0), iknots = x$iknots[-j], bknots = x$bknots, order = x$order)
               }
       )

    if (verbose) {
      message("  generating coarsened control polygons (step 3 of 6)")
    }

    coarsened_cps <- MAP(FUN = cp, x = coarsened_bmats, theta = coarsened_thetas)

    bmat0 <- bsplines(numeric(0), iknots = x$iknots, bknots = x$bknots, order = x$order)

    if (verbose) {
      message("  building theta hat (step 4 of 6)")
    }

    if (isTRUE(nrow(x$vcov_theta) > 0L)) {
      hat_thetas <-
        LAPPLY(
            X = seq(x$order, x$order + length(x$iknots) - 1)
          , FUN = hat_theta
          , xi = x$xi
          , k = x$order
          , theta = x$cp$theta
      )
    } else {
      hat_thetas <-
        LAPPLY(
            X = seq(x$order, x$order + length(x$iknots) - 1)
          , FUN = hat_theta
          , xi = x$xi
          , k = x$order
          , theta = x$cp$theta
      )
    }

    if (verbose) {
      message("  building restored control polygons (step 5 of 6)")
    }

    restored_cps <-
      MAP(FUN = cp
          , x =
            lapply(1:length(hat_thetas), function(x) bmat0)
          , theta = lapply(hat_thetas, getElement, "theta")
      )

    # p-values
    if (verbose) {
      message("  building test statistics (step 6 of 6)")
    }
    if (calculate_test_statistic & !is.null(x$vcov_theta)) {

      # js are indexed for cpp not R
      js <- seq(x$order, x$order + length(x$iknots) - 1, by = 1L)

      chisq <-
        LAPPLY(js
               , test_statistic
               , xi    = x$xi
               , k     = x$order
               , theta = x$theta
               , Sigma = x$vcov_theta
               , cl = cl)
      chisq <- do.call(c, chisq)

    } else {
      chisq <- rep(NA_real_, length(x$iknots))
    }


    rtn <- list(
                original_cp   = x,
                coarsened_cps = coarsened_cps,
                restored_cps  = restored_cps,
                d             = lapply(hat_thetas, getElement, "d"),
                influence     = sapply(hat_thetas, getElement, "influence"),
                chisq         = chisq
                )
  } else {
    # no internal knots
    rtn <- list(
                original_cp   = x,
                coarsened_cps = NA,
                restored_cps  = NA,
                d             = NA,
                influence     = NA,
                chisq         = NA
                )
  }

  class(rtn) <- "cpr_influence_of_iknots"

  rtn
}

#' @export
influence_of_iknots.cpr_cpr <- function(x, verbose = FALSE, cl = 2L, calculate_test_statistic = TRUE, ...) {
  rtn <- lapply(x, influence_of_iknots, verbose = verbose, cl = cl, calculate_test_statistic = calculate_test_statistic)#, ...)
  class(rtn) <- c("cpr_influence_of_iknots_cpr", class(rtn))
  rtn
}

#' @param margin which margin(s) to consider the influence of iknots
#' @param n_polycoef number of polynomial coefficients to use when assessing the
#' influence of a iknot
#' @rdname influence_of_iknots
#' @export
influence_of_iknots.cpr_cn <- function(x, verbose = FALSE, cl = 2L, margin = seq_along(x$bspline_list), n_polycoef = 20L, ...) {

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  xvecs <-
    mapply(seq,
           from = lapply(bknots, min),
           to   = lapply(bknots, max),
           MoreArgs = list(length = n_polycoef),
           SIMPLIFY = FALSE)

  xvecs <-
    lapply(xvecs, function(x) { x[length(x)] <- x[length(x)] - sqrt(.Machine$double.eps) })

  marginal_bsplines <-
    mapply(bsplines,
           x = xvecs,
           iknots = iknots,
           bknots = bknots,
           order  = orders,
           SIMPLIFY = FALSE)

  marginal_tensors <- lapply(seq_along(marginal_bsplines),
           function(idx) {
             do.call(build_tensor, marginal_bsplines[-idx])
           })

  marginal_thetas <-
    lapply(seq_along(x$bspline_list),
           function(m) {
             apply(array(x$cn$theta, dim = dfs), m, function(x) x)
           })

  polynomial_coef <-
    mapply(function(xx, yy) {t(xx %*% yy)},
           xx = marginal_tensors,
           yy = marginal_thetas,
           SIMPLIFY = FALSE)

  wghts <-
    lapply(seq_along(x$bspline_list)[margin],
           function(idx) {
             lapply(split(polynomial_coef[[idx]], col(polynomial_coef[[idx]])),
                    function(tt, bmat) {
                      influence_of_iknots(cp(bmat, tt), verbose = verbose, cl = cl, ...)
                    },
                    bmat = x$bspline_list[[idx]])
           })

  wghts <- lapply(wghts, getElement, 1)
  class(wghts) <- c("cpr_influence_of_iknots_cpn", class(wghts))
  wghts
}


#' @export
print.cpr_influence_of_iknots <- function(x, ...) {
  if (length(x$original_cp$iknots) > 0L) {
    print(stats::setNames(x$influence,
                   paste0("xi_", x$original_cp$order +
                   seq(1, length(x$original_cp$iknots), by = 1)))
    )
  } else {
    message("no internal knots")
  }
  invisible(x)
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

  plots <- lapply(plots, function(g) {
                    cp_colors <- c("Original" = "#A2AAAD", "Coarsened" = "#236192", "Restored" = "#6F263D")
                    cp_pch    <- c("Original" = 1,         "Coarsened" = 2,         "Restored" = 3)
                    cp_lty    <- c("Original" = 1,         "Coarsened" = 2,         "Restored" = 3)
                    g +
                      ggplot2::theme_bw() +
                      ggplot2::theme(axis.title = ggplot2::element_blank()) +
                      ggplot2::scale_color_manual(name = "", values = cp_colors, labels = scales::parse_format()) +
                      ggplot2::scale_linetype_manual(name = "", values = cp_lty, labels = scales::parse_format()) +
                      ggplot2::scale_shape_manual(name = "", values = cp_pch, labels = scales::parse_format())
        })

  if (length(j) == 1) {
    plots[[1]]
  } else {
    plots
  }
}

#' @export
summary.cpr_influence_of_iknots <- function(object, ...) {

  iknots <- object$original_cp$iknots
  j = object$original_cp$order + seq_along(object$original_cp$iknots)
  if (length(iknots) == 0L) {
    iknots <- NA
    j <- object$original_cp$order
  }

  rtn <-
    data.frame(
                 j = j
               , iknot = iknots
               , influence = object$influence
               , influence_rank = rank(object$influence, ties.method = "first", na.last = "keep")
               , chisq = object$chisq
               , chisq_rank = rank(object$chisq, ties.method = "first", na.last = "keep")
               , p_value = 1.0 - stats::pchisq(object$chisq, df = 1)
               )

  rtn$os_p_value = 1 -
    p_order_statistic(q = rtn$chisq
                      , n = length(rtn$chisq)
                      , j = rtn$chisq_rank
                      , distribution = "chisq"
                      , df = 1)

  class(rtn) <- c("cpr_influence_of_iknots_summary", class(rtn))

  rtn
}

#' @export
summary.cpr_influence_of_iknots_cpr <- function(object, ...) {
  rtn <- lapply(object, summary)
  rws <- sapply(rtn, nrow)
  idx <- rep(rws + 1, times = rws)
  rtn <- do.call(rbind, rtn)
  rtn$index <- idx
  class(rtn) <- c("cpr_influence_of_iknots_cpr_summary", class(rtn))
  rtn
}

#' @export
summary.cpr_influence_of_iknots_cpn <- function(object, ...) {
  rtn <- lapply(object, summary)
  rws <- sapply(rtn, nrow)
  idx <- rep(rws + 1, times = rws)
  rtn <- do.call(rbind, rtn)
  rtn$index <- idx
  rtn$margin <- rep(seq(1, length(rws)), times = rws)
  names(rtn)[names(rtn) == "influence_rank"] <- "marginal_influence_rank"
  rtn$influence_rank <- rank(rtn$influence, ties.method = "first", na.last = "keep")
  class(rtn) <- c("cpr_influence_of_iknots_cpn_summary", class(rtn))
  rtn
}

#' @export
print.cpr_influence_of_iknots_cpr_summary <- function(x, ...) {
  if (nrow(x) == 0) {
    message("no internal knots")
    return(invisible(x))
  }

  if (all(is.na(x$chisq) )) {
    print.data.frame(x[, c("j", "iknot", "influence", "influence_rank")])
  } else {
    NextMethod(x)
  }
  invisible(x)
}

#' @export
print.cpr_influence_of_iknots_cpn_summary <- function(x, ...) {
  if (nrow(x) == 0) {
    message("no internal knots")
    return(invisible(x))
  }

  if (all(is.na(x$chisq) )) {
    print.data.frame(x[, c("margin", "j", "iknot", "influence", "marginal_influence_rank", "influence_rank")])
  } else {
    NextMethod(x)
  }
  invisible(x)
}
