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
  llk_elbow <- rss_elbow <- rse_elbow <- numeric(0)
  for (brkpt in seq(1, length(object[[length(object)]]$iknot), by = 1)) {
    rse_y <- suppressWarnings(cp(rse ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
  #  rss_y <- suppressWarnings(cp(rss ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
    llk_y <- suppressWarnings(cp(loglik ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 3), data = rtn))
    rse_elbow <- c(rse_elbow, rse_y$rse)
    #rss_elbow <- c(rss_elbow, rss_y$rse)
    llk_elbow <- c(llk_elbow, llk_y$rse)
  }
  rse_elbow <- which.min(rse_elbow) + 1
  #rss_elbow <- which.min(rss_elbow) + 1
  llk_elbow <- which.min(llk_elbow) + 1
  rtn$loglik_elbow <- as.integer(rtn$n_iknots == llk_elbow)
  #rtn$rss_elbow <- as.integer(rtn$n_iknots == rss_elbow)
  rtn$rse_elbow <- as.integer(rtn$n_iknots == rse_elbow)

  class(rtn) <- c("cpr_cpr_summary", class(rtn))
  rtn
}

#' @export
print.cpr_cpr_summary <- function(x, ...) {
  y <- x

  dig.tst = 5L#max(1L, min(5L, digits - 1L))
  eps.Pvalue = .Machine$double.eps
  y[["Pr(>w_(1))"]] <- format.pval(y[["Pr(>w_(1))"]] , digits = dig.tst, eps = eps.Pvalue)

  y[["loglik_elbow"]] <- sub("0", "", sub("1", "<<<", as.character(y[["loglik_elbow"]])))
  #y[["rss_elbow"]] <- sub("0", "", sub("1", "<<<", as.character(y[["rss_elbow"]])))
  y[["rse_elbow"]] <- sub("0", "", sub("1", "<<<", as.character(y[["rse_elbow"]])))
  print.data.frame(y)

  invisible(x)
}
