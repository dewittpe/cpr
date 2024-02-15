#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.
#'
#' The algorithm is generally speaking fast, but can take a long time to run if
#' the number of interior knots of initial control polygon is high.  To help
#' track the progress of the execution you can have \code{progress = "cpr"}
#' which will show a progress bar incremented for each iteration of the CPR
#' algorithm.  \code{progress = "influence"} will use a combination of messages
#' and progress bars to report on each step in assessing the influence of all the
#' internal knots for each iteration of the CPR algorithm.  See
#' \code{\link{influence_of_iknots}} for more details.
#'
#' @param x a \code{cpr_cp} object
#' @param progress controls the level of progress messaging.  See Details.
#' @param ... not currently used
#'
#' @return a list of \code{cpr_cp} objects
#'
#' @seealso \code{\link{influence_of_iknots}}
#'
#' @examples
#' #############################################################################
#' # Example 1: find a model for log10(pdg) = f(day) from the spdg data set
#' \donttest{
#' # need the lme4 package to fit a mixed effect model
#' require(lme4)
#'
#' # construct the initial control polygon.  Forth order spline with fifty
#' # internal knots.  Remember degrees of freedom equal the polynomial order
#' # plus number of internal knots.
#' init_cp <- cp(log10(pdg) ~ bsplines(day, df = 24, bknots = c(-1, 1)) + (1|id),
#'               data = spdg, method = lme4::lmer)
#' cpr_run <- cpr(init_cp)
#' plot(cpr_run, color = TRUE)
#'
#' s <- summary(cpr_run)
#' s
#' plot(s, type = "rse")
#'
#' # preferable model is in index 5 by eye
#' preferable_cp <- cpr_run[["cps"]][[5]]
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
#' init_cp <- cp(formula = y ~ bsplines(x, df = 24, bknots = c(0, 4.5)),
#'               data    = sim_data,
#'               method  = glm,
#'               method.args = list(family  = binomial())
#'               )
#'
#' # run CPR
#' cpr_run <- cpr(init_cp)
#'
#' # preferable model is in index 6
#' s <- summary(cpr_run)
#' plot(s, color = TRUE, type = "rse")
#'
#' plot(
#'     cpr_run
#'   , color = TRUE
#'   , from = 5
#'   , to = 7
#'   , show_spline = TRUE
#'   , show_cp = FALSE
#'   )
#'
#'
#' # plot the fitted spline and the true p(x)
#' sim_data$pred_select_p <- plogis(predict(cpr_run[[7]], newdata = sim_data))
#' ggplot2::ggplot(sim_data) +
#' ggplot2::theme_bw() +
#' ggplot2::aes(x = x) +
#' ggplot2::geom_point(mapping = ggplot2::aes(y = y), alpha = 0.1) +
#' ggplot2::geom_line(
#'     mapping = ggplot2::aes(y = pred_select_p, color = "pred_select_p")
#'   ) +
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
#' ggplot2::geom_line(
#'     mapping = ggplot2::aes(y = pred_select_p, color = "pred_select_p")
#'   ) +
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
cpr <- function(x, progress = c('cpr', 'influence', 'none'), ...) {
  UseMethod("cpr")
}

#' @export
cpr.cpr_cp <- function(x, progress = c('cpr', 'influence', 'none'), ...) {

  progress <- match.arg(progress, several.ok = FALSE)

  ioik_out <- cp_out <- vector("list", length = length(x$iknots) + 1L)

  if (progress == 'cpr') {
    pb <- utils::txtProgressBar(max = length(cp_out), style = 3) # nocov
    prg <- 0 # noocv
    utils::setTxtProgressBar(pb, prg) # noocv
  }

  for(i in rev(seq_along(cp_out)[-1])) {
    cp_out[[i]] <- x
    w <- summary(influence_of_iknots(cp_out[[i]], verbose = (progress == 'influence'), ...))
    nkts <- w$iknot[ w$influence_rank > 1 ]
    ioik_out[[i]] <- w

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

  cp_out[[1]]   <- x
  ioik_out[[1]] <- summary(influence_of_iknots(x))

  if (progress == 'cpr') {
    utils::setTxtProgressBar(pb, prg <- prg + 1) # nocov
    close(pb) # nocov
  }

  attr(cp_out, "ioik") <- ioik_out

  class(cp_out) <- c("cpr_cpr", class(cp_out))
  cp_out
}

#' @method print cpr_cpr
#' @export
print.cpr_cpr <- function(x, ...) {
  cat("A list of control polygons\n")
  cat(utils::str(x, max.level = 0))
  invisible(x)
}
