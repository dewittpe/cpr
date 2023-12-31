#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.
#'
#' @param x a \code{cpr_bs} object
#' @param ... pass through
#'
#' @return a \code{cpr_cp} object, this is a list with the element \code{cp}, a
#' data.frame reporting the x and y coordinates of the control polygon.
#' Additional elements include the knot sequence, polynomial order, and other
#' meta data regarding the construction of the control polygon.
#'
#' @examples
#'
#' # Support
#' xvec <- runif(n = 500, min = 0, max = 6)
#' bknots <- c(0, 6)
#'
#' # Define the basis matrix
#' bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = bknots)
#' bmat2 <- bsplines(x = xvec, bknots = bknots)
#'
#' # Define the control vertices ordinates
#' theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
#' theta2 <- c(1, 3.4, -2, 1.7)
#'
#' # build the two control polygons
#' cp1 <- cp(bmat1, theta1)
#' cp2 <- cp(bmat2, theta2)
#'
#' # black and white plot
#' plot(cp1)
#' plot(cp1, show_spline = TRUE)
#'
#' # multiple control polygons
#' plot(cp1, cp2, show_spline = TRUE)
#' plot(cp1, cp2, color = TRUE)
#' plot(cp1, cp2, show_spline = TRUE, color = TRUE)
#'
#' # via formula
#' DF  <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))
#' cp3 <- cp(y ~ bsplines(x, bknots = bknots), data = DF)
#'
#' # plot the spline and target data.
#' plot(cp3, show_cp = FALSE, show_spline = TRUE) +
#'   ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, color = "Target"),
#'                      data = DF, linetype = 2)
#'
#' @export
cp <- function(x, ...) {
  UseMethod("cp")
}

#' @export
#' @rdname cp
#' @param theta a vector of (regression) coefficients, the ordinates of the control polygon.
cp.cpr_bs <- function(x, theta, ...) {
  out <- list(cp = data.frame(xi_star = as.numeric(attr(x, "xi_star")),
                              theta   = as.vector(theta)),
              xi = c(attr(x, "xi")),
              iknots = c(attr(x, "iknots")),
              bknots = c(attr(x, "bknots")),
              order  = attr(x, "order"),
              call   = match.call(),
              keep_fit = NULL,
              fit    = NULL,
              theta = NULL,
              theta_vcov = NULL,
              coefficients = NULL,
              vcov = NULL,
              vcov_theta = NULL,
              loglik = NULL,
              rss    = NULL,
              rse    = NULL)

  class(out) <- c("cpr_cp", class(out))

  out
}

#' @export
#' @rdname cp
#' @param formula a formula that is appropriate for regression method being used.
#' @param data a required \code{data.frame}
#' @param method the regression method such as \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, etc.
#' @param method.args a list of additional arguments to pass to the regression method.
#' @param keep_fit (logical, default value is \code{TRUE}).  If \code{TRUE} the regression model fit is retained and returned in as the \code{fit} element. If \code{FALSE} the \code{fit} element with be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if \code{TRUE} check that the design matrix is full rank.
cp.formula <- function(formula, data, method = stats::lm, method.args = list(), keep_fit = TRUE, check_rank = TRUE, ...) {

  # check for some formula specification issues
  rhs_check <- grepl("bsplines", attr(stats::terms(formula), "term.labels"))
  if ( !rhs_check[1] | any(rhs_check[-1]) ) {
    stop("bsplines() must appear first, once, and with no effect modifiers, as the first term on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  cl <- list(formula = as.name("f_for_use"),
             data = as.name("data_for_use"))
  cl <- c(cl, method.args)

  regression <- match.fun(method)
  fit <- do.call(regression, cl)
  COEF_VCOV <- coef_vcov(fit)

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m) | any(is.na(COEF_VCOV$coef))) {
      keep_fit <- TRUE
      warning("Design Matrix is rank deficient. keep_fit being set to TRUE.")
    }
  }

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cp")
  cl <- as.call(cl)

  Bmat <- stats::model.frame(fit)
  Bmat <- Bmat[[which(grepl("bsplines", names(Bmat)))]]

  out <- cp.cpr_bs(Bmat, as.vector(COEF_VCOV$theta))

  # update elements of the cpr_bs object
  if (keep_fit) {
    out[["fit"]] <-  fit
  }

  out[["call"]]         <- cl
  out[["keep_fit"]]     <- keep_fit
  out[["theta"]]        <- COEF_VCOV$theta
  out[["vcov_theta"]]   <- COEF_VCOV$vcov_theta
  out[["coefficients"]] <- COEF_VCOV$coef
  out[["vcov"]]         <- COEF_VCOV$vcov
  out[["loglik"]]       <- loglikelihood(fit)
  out[["rss"]]          <- sum(stats::residuals(fit)^2)
  out[["rse"]]          <- sqrt(sum(stats::residuals(fit)^2) / (nrow(data) - length(COEF_VCOV$coef)))

  out
}

#' @export
print.cpr_cp <- function(x, ...) {
  print(x$cp, ...)
  invisible(x)
}
