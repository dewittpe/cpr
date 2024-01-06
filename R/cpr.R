#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.
#'
#' The algorithm is genarlly speaking fast, but can take a long time to run if
#' the number of interior knots of initial control polygon is high.  To help
#' track the progress of the execution you can have \code{progress = "cpr"}
#' which will show a progress bar incremented for each interation of the CPR
#' algorithm.  \code{progress = "influence"} will use a conbination of messages
#' and progress bars to report on each step in assessing the influce of all the
#' internal knots for each iteration of the CPR algorithm.  See
#' \code{\link{influence_of_iknots}} for more details.
#'
#' @param x a \code{cpr_cp} object
#' @param progress controls the level of progress messaging.  See Details.
#' @param cl passed to \code{\link[pbapply]{pblapply}} or
#' \code{\link[parallel]{mclapply}} depending on the level of \code{progress}.
#' See Details.
#' @param ... not currently used
#'
#' @return a list of \code{cpr_cp} objects
#'
#' @seealso \code{\link{influence_of_iknots}}
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
#' s <- summary(cpr_run)
#' s
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
#' s <- summary(cpr_run)
#' s
#'
#' plot(s, color = TRUE, type = "rse") # same plot as plot(cpr_run, color = TRUE, type = "rse")
#'
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
cpr <- function(x, progress = c('cpr', 'influence', 'none'), cl = NULL, ...) {
  UseMethod("cpr")
}

#' @export
cpr.cpr_cp <- function(x, progress = c('cpr', 'influence', 'none'), cl = NULL, ...) {

  progress <- match.arg(progress, several.ok = FALSE)

  out <- vector("list", length = length(x$iknots) + 1L)

  if (progress == 'cpr') {
    pb <- utils::txtProgressBar(max = length(out), style = 3) # nocov
    prg <- 0 # noocv
    utils::setTxtProgressBar(pb, prg) # noocv
  }

  for(i in rev(seq_along(out)[-1])) {
    out[[i]] <- x
    w <- summary(influence_of_iknots(out[[i]], verbose = (progress == 'influence'), cl = cl, ...))
    nkts <- w$iknot[ w$influence_rank > 1 ]

    x <-
      eval(
        stats::update(
             x
           , formula = newknots(x$call$formula, nkts)
           , keep_fit = TRUE
           , check_rank = FALSE
           , evaluate = FALSE)
        , parent.frame()
      )

    if (progress == 'cpr') {
      utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    }

  }

  out[[1]] <- x

  if (progress == 'cpr') {
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
  cat(utils::str(x, max.level = 0))
  invisible(x)
}
