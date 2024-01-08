#' Summarize a Control Polygon Reduction Object
#'
#' @param object a \code{cpr_cpr} object
#' @param ... pass through
#'
#' @return a \code{data.frame} with the attribute \code{elbow} which is a
#' programmatic attempt to identify a useful trade-off between degrees of freedom
#' and fit statistic.
#'
#' @examples
#'
#' set.seed(42)
#' x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
#' DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))
#'
#' initial_cp <-
#'   cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
#'      , data = DF
#'      , keep_fit = TRUE # default is FALSE
#'   )
#'
#' cpr0 <- cpr(initial_cp)
#' s <- summary(cpr0)
#' s
#' plot(s, type = "rse")
#'
#' @export
summary.cpr_cpr <- function(object, estimate_elbow = TRUE, ...) {

  rtn <- lapply(object[["cps"]], summary)
  rtn <- do.call(rbind, rtn)

  selected_index <- do.call(rbind, object[["ioik"]])
  selected_index <- selected_index$os_p_value[selected_index$chisq_rank == 1]
  rtn[["Pr(>w_(1))"]] <- selected_index

  # find the elbow in the rse by n_iknots plot
  elbow <- matrix(numeric(0), nrow = nrow(rtn), ncol = 6)

  if (estimate_elbow) {
    for (brkpt in seq(0, nrow(rtn), by = 1)) {
      rse3 <- suppressWarnings(cp(rse ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, nrow(rtn)), order = 3), data = rtn))
      rss3 <- suppressWarnings(cp(rss ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, nrow(rtn)), order = 3), data = rtn))
      llk3 <- suppressWarnings(cp(loglik ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, nrow(rtn)), order = 3), data = rtn))
      rse2 <- suppressWarnings(cp(rse ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, nrow(rtn)), order = 2), data = rtn))
      rss2 <- suppressWarnings(cp(rss ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, nrow(rtn)), order = 2), data = rtn))
      llk2 <- suppressWarnings(cp(loglik ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, nrow(rtn)), order = 2), data = rtn))
      elbow[brkpt, ] <-
        c(
          llk3 = llk3$rse,
          rss3 = rss3$rse,
          rse3 = rse3$rse,
          llk2 = llk2$rse,
          rss2 = rss2$rse,
          rse2 = rse2$rse
          )
    }
    elbow <- matrix(apply(elbow, 2, which.min) + 1, byrow = TRUE, ncol = 3)
    dimnames(elbow) <- list(c("quadratic", "linear"), c("loglik", "rss", "rse"))
  }

  attr(rtn, "elbow") <- elbow

  class(rtn) <- c("cpr_summary_cpr_cpr", class(rtn))
  rtn
}

#' @export
print.cpr_summary_cpr_cpr <- function(x, n = 6, ...) {
  y <- x

  dig.tst = 5L#max(1L, min(5L, digits - 1L))
  eps.Pvalue = .Machine$double.eps
  y[["Pr(>w_(1))"]] <- format.pval(y[["Pr(>w_(1))"]] , digits = dig.tst, eps = eps.Pvalue)

  if (n < nrow(y) - 1L) {
    y <- rbind(utils::head(y, n = ceiling(n/2)),
               "---" = "",
               utils::tail(y, n = ceiling(n/2)))
  }

  print.data.frame(y)

  cat("\n-------\nElbows (index of selected model):\n")
  print(attr(x, "elbow"))

  invisible(x)
}
