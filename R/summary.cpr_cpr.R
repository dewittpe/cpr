#' Summarize a Control Polygon Reduction Object
#'
#' @param object a \code{cpr_cpr} object
#' @param .... pass through
#'
#' @return
#'
#' @examples
#'
#' @export
summary.cpr_cpr <- function(object, ...) {

  rtn <- lapply(object, summary)
  rtn <- do.call(rbind, rtn)

  selected_index <- summary(influence_of_iknots(object))
  selected_index <- selected_index$os_p_value[selected_index$chisq_rank == 1]
  selected_index <- c(NA_real_, selected_index)
  rtn[["Pr(>w_(1))"]] <- selected_index

  # find the elbow in the rse by n_iknots plot
  elbow <- matrix(numeric(0), nrow = length(object[[length(object)]]$iknot), ncol = 6)
  for (brkpt in seq(0, length(object[[length(object)]]$iknot), by = 1)) {
    rse3 <- suppressWarnings(cp(rse ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
    rss3 <- suppressWarnings(cp(rss ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
    llk3 <- suppressWarnings(cp(loglik ~ bsplines(n_iknots, iknot = c(brkpt, brkpt), bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
    rse2 <- suppressWarnings(cp(rse ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 2), data = rtn))
    rss2 <- suppressWarnings(cp(rss ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 2), data = rtn))
    llk2 <- suppressWarnings(cp(loglik ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 2), data = rtn))
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
    y <- rbind(head(y, n = ceiling(n/2)),
               "---" = "",
               tail(y, n = ceiling(n/2)))
  }

  print.data.frame(y)

  cat("\n-------\nElbows (index of selected model):\n")
  print(attr(x, "elbow"))

  invisible(x)
}
