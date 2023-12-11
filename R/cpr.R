#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.
#'
#' \code{keep} will keep the regression fit as part of the \code{cpr\_cp} object
#' for models with up to and including keep fits.  For example, if \code{keep =
#' 10} then the resulting \code{cpr\_cpr} object will have the regression fit
#' stored in the first \code{keep + 1} (zero internal knots, one internal knot,
#' \ldots, \code{keep} internal knots) \code{cpr\_cp} objects in the list.  The
#' limit on the number of stored regression fits is to keep memory usage down.
#'
#' @param x a \code{cpr_cp} object
#' @param keep keep (store) the regression fit for models with \code{keep} or
#' fewer internal knots, e.g., \code{keep = 3} will result in the regression fit
#' for models with 0, 1, 2, and 3 internal knots being saved in their respective
#' \code{cpr_cp} objects.  The default is \code{keep = -1} so that no regression
#' models are retained.
#' @param p defaults to 2L, the L^p norm used in determining the influence
#'        weight of each internal knot.
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
#' init_cp <- cp(log10(pdg) ~ bsplines(day, df = 54) + (1|id),
#'               data = spdg, method = lme4::lmer)
#' cpr_run <- cpr(init_cp)
#' plot(cpr_run, color = TRUE)
#' plot(cpr_run, type = "rmse", to = 10)
#'
#' # preferable model is in index 4
#' preferable_cp <- cpr_run[[4]]
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
#'               family  = binomial(),
#'               method  = glm,
#'               data    = sim_data)
#'
#' # run CPR, preferable model is in index 7
#' cpr_run <- cpr(init_cp)
#' plot(cpr_run, color = TRUE, type = "rmse", to = 15)
#' plot(cpr_run, color = TRUE, from = 11, to = 15, show_spline = TRUE)
#'
#' # plot the fitted spline and the true p(x)
#' sim_data$pred_select_p <-
#'   plogis(predict(cpr_run[[7]], newdata = sim_data)$pred)
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
cpr <- function(x, keep = -1, p = 2, progress = interactive(), ...) {
  UseMethod("cpr")
}

#' @export
cpr.cpr_cp <- function(x, keep = -1, p = 2, progress = interactive(), ...) {

  out <- vector("list", length = length(x$iknots) + 1L)

  if ((length(out) > (keep + 1)) && (x$keep_fit)) {
    x <- eval(stats::update(x, keep_fit = FALSE, check_rank = FALSE, evaluate = FALSE), parent.frame())
  } else if (length(out) <= (keep + 1) & !x$keep_fit) {
    x <- eval(stats::update(x, keep_fit = TRUE, check_rank = FALSE, evaluate = FALSE), parent.frame())
  }

  if (progress) {
    pb <- utils::txtProgressBar(max = length(out), style = 3)
    prg <- 0
    utils::setTxtProgressBar(pb, prg)
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- summary(influence_of_iknots(out[[i]]))
    nkts <- w$iknot[ w$influence_rank > 1 ]

    if (i == keep + 1) {
      x <- stats::update(x, keep_fit = TRUE)
    }

    x <- eval(stats::update(x, formula = newknots(x$call$formula, nkts), check_rank = FALSE, evaluate = FALSE), parent.frame())

    if (progress) {
      utils::setTxtProgressBar(pb, prg <- prg + 1)
    }

  }

  out[[1]] <- x

  if (progress) {
    utils::setTxtProgressBar(pb, prg <- prg + 1)
    close(pb)
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
  #for (i in seq_along(object)) {
  #  rtn[[i]]$index <- as.integer(i)
  #}
  rtn <- do.call(rbind, rtn)

  selected_index <- summary(influence_of_iknots(object))
  selected_index <- selected_index$os_p_value[selected_index$chisq_rank == 1]
  selected_index <- c(NA_real_, selected_index)
  rtn[["Pr(>w_(1))"]] <- selected_index

  # find the elbow in the rmse by n_iknots plot
  elbow <- numeric(0)
  for (brkpt in seq(1, length(object[[length(object)]]$iknot), by = 1)) {
    y <- cp(rmse ~ bsplines(n_iknots, iknot = brkpt, bknots = c(0, length(object[[length(object)]]$iknot)), order = 3),
            data = rtn)
    elbow <- c(elbow, y$rmse)
  }
  elbow <- which.min(elbow) + 1
  rtn$elbow <- as.integer(rtn$n_iknots == elbow)

  class(rtn) <- c("cpr_cpr_summary", class(rtn))
  rtn
}

#' @export
print.cpr_cpr_summary <- function(x, ...) {
  y <- x
  #y[["Pr(>w_(1))"]] <- qwraps2::frmtp(y[["Pr(>w_(1))"]])
  pv <- y[["Pr(>w_(1))"]]
  sstars <- stats::symnum(pv, corr = FALSE, na = TRUE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
  sleg <- attr(sstars, "legend")
  sstars <- as.character(sstars)

  ms <- max(sapply(sstars, nchar))
  for (i in seq_along(sstars)) {
    if (sstars[i] < ms) {
      sstars[i] <- paste(sstars[i], paste(rep(" ", ms - nchar(sstars[i])), collapse = ""))
    }
  }

  dig.tst = 5L#max(1L, min(5L, digits - 1L))
  eps.Pvalue = .Machine$double.eps
  y[["Pr(>w_(1))"]] <- format.pval(pv, digits = dig.tst, eps = eps.Pvalue)
  y[["Pr(>w_(1))"]] <- paste(y[["Pr(>w_(1))"]], sstars)

  y[["elbow"]] <- sub("0", "", sub("1", "<<<", as.character(y[["elbow"]])))
  print.data.frame(y)
  #NextMethod(generic = "print", object = y)

  if ((w <- getOption("width")) < nchar(sleg))
    sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
  cat("---\nSignif. codes:  ", sleg, sep = "", fill = w +
      4 + max(nchar(sleg, "bytes") - nchar(sleg)))
  invisible(x)
}

