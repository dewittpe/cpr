#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.
#'
#' @param x a \code{cpr_cp} object
#' @param progress show a progress bar.
#' @param ... not currently used
#'
#' @examples
#' #############################################################################
#' # Example 1: find a model for log10(pdg) = f(day) from the spdg data set
#' \dontrun{
#' # need the lme4 package to fit a mixed effect model
#' require(lme4)
#'
#' # construct the initial control polygon.  Forth order spline with fifty
#' # internal knots.  Remember degrees of freedom equal the polynomial order
#' # plus number of internal knots.
#' init_cp <- cp(log10(pdg) ~ bsplines(day, df = 54, bknots = c(-1, 1)) + (1|id),
#'               data = spdg, method = lme4::lmer)
#' cpr_run <- cpr(init_cp)
#' plot(cpr_run, color = TRUE)
#' plot(cpr_run, type = "rse")
#'
#' summary(cpr_run)
#'
#' # preferable model is in index 5
#' preferable_cp <- cpr_run[[5]]
#' }
#'
#' #############################################################################
#' # Example 2: logistic regression
#' # simulate a binary response Pr(y = 1 | x) = p(x)
#' p <- function(x) { 0.65 * sin(x * 0.70) + 0.3 * cos(x * 4.2) }
#'
#' set.seed(42)
#' x <- runif(2500, 0.00, 4.5)
#' sim_data <- data.frame(x = x, y = rbinom(2500, 1, p(x)))
#'
#' # Define the initial control polygon
#' init_cp <- cp(formula = y ~ bsplines(x, df = 54, bknots = c(0, 4.5)),
#'               data    = sim_data,
#'               method  = glm,
#'               method.args = list(family  = binomial())
#'               )
#'
#' # run CPR, preferable model is in index 7
#' cpr_run <- cpr(init_cp)
#'
#' summary(cpr_run)
#'
#' plot(cpr_run, color = TRUE, type = "rse")
#' plot(cpr_run, color = TRUE, from = 5, to = 9, show_spline = TRUE)
#'
#' # plot the fitted spline and the true p(x)
#' sim_data$pred_select_p <- plogis(predict(cpr_run[[7]], newdata = sim_data))
#'
#' ggplot2::ggplot(sim_data) +
#' ggplot2::theme_bw() +
#' ggplot2::aes(x = x) +
#' ggplot2::geom_point(mapping = ggplot2::aes(y = y), alpha = 0.1) +
#' ggplot2::geom_line(mapping = ggplot2::aes(y = pred_select_p, color = "pred_select_p")) +
#' ggplot2::stat_function(fun = p, mapping = ggplot2::aes(color = 'p(x)'))
#'
#' # compare to gam and a binned average
#' sim_data$x2 <- round(sim_data$x, digits = 1)
#' bin_average <-
#'   lapply(split(sim_data, sim_data$x2), function(x) {
#'            data.frame(x = x$x2[1], y = mean(x$y))
#'          })
#' bin_average <- do.call(rbind, bin_average)
#'
#' ggplot2::ggplot(sim_data) +
#' ggplot2::theme_bw() +
#' ggplot2::aes(x = x) +
#' ggplot2::stat_function(fun = p, mapping = ggplot2::aes(color = 'p(x)')) +
#' ggplot2::geom_line(mapping = ggplot2::aes(y = pred_select_p, color = "pred_select_p")) +
#' ggplot2::stat_smooth(mapping = ggplot2::aes(y = y, color = "gam"),
#'                      method = "gam",
#'                      formula = y ~ s(x, bs = "cs"),
#'                      se = FALSE,
#'                      n = 1000) +
#' ggplot2::geom_line(data = bin_average
#'                    , mapping = ggplot2::aes(y = y, color = "bin_average"))
#'
#'
#' @export
cpr <- function(x, progress = interactive(), ...) {
  UseMethod("cpr")
}

#' @export
cpr.cpr_cp <- function(x, progress = interactive(), ...) {

  out <- vector("list", length = length(x$iknots) + 1L)

  if (progress) {
    pb <- utils::txtProgressBar(max = length(out), style = 3) # nocov
    prg <- 0 # noocv
    utils::setTxtProgressBar(pb, prg) # noocv
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- summary(influence_of_iknots(out[[i]]))
    nkts <- w$iknot[ w$influence_rank > 1 ]

    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), keep_fit = TRUE, check_rank = FALSE, evaluate = FALSE), parent.frame())

    if (progress) {
      utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    }

  }

  out[[1]] <- x

  if (progress) {
    utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    close(pb) # nocov
  }

  class(out) <- c("cpr_cpr", class(out))
  out
}

#' @method print cpr_cpr
#' @export
print.cpr_cpr <- function(x, ...) {
  cat("A list of control polygons\n")
  utils::str(x, max.level = 0)
}

#' @export
#' @param object a \code{cpr_cpr} object
#' @rdname cpr
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
