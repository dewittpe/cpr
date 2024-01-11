#' Control Nets
#'
#' Generate the control net for a uni-variable B-spline
#'
#' \code{cn} generates the control net for the given B-spline function.  There
#' are several methods for building a control net.
#'
#' @param x a \code{cpr_bt} object
#' @param ... pass through
#'
#' @return a \code{cpr_cn} object.  This is a list with the following elements.
#' Some of the elements are omitted when the using the \code{cn.cpr_bt} method.
#' \describe{
#'  \item{cn}{the control net, \code{data.frame} with each row defining a vertex
#'  of the control net}
#'  \item{bspline_list}{A list of the marginal B-splines}
#'  \item{call}{the call}
#'  \item{keep_fit}{logical, indicates if the regression models was retained}
#'  \item{fit}{if \code{isTRUE(keep_fit)} then the regression model is here,
#'  else \code{NA}.}
#'  \item{coefficients}{regression coefficients, only the fixed effects if a
#'  mixed effects model was used.}
#'  \item{vcov}{The variance-covariance matrix for the \code{coefficients}}
#'  \item{loglik}{The log-likelihood for the regression model}
#'  \item{rse}{the residual standard error for the regression models}
#'  }
#'
#' @seealso \code{\link{summary.cpr_cn}}, \code{\link{cnr}}
#'
#' @examples
#'
#' acn <- cn(log10(pdg) ~ btensor(  x = list(day, age)
#'                                 , df = list(30, 4)
#'                                 , bknots = list(c(-1, 1), c(44, 53)))
#'            , data = spdg)
#'
#' # plot3D
#' plot(acn, rgl = FALSE)
#'
#'
#' @export
cn <- function(x, ...) {
  UseMethod("cn")
}

#' @export
#' @rdname cn
#' @param theta a vector of (regression) coefficients, the ordinates of the
#' control net.
cn.cpr_bt <- function(x, theta, ...) {
  xi_stars <- lapply(attr(x, "bspline_list"), attr, which = "xi_star")

  out <-
    list(cn      = data.frame(cbind(do.call(expand.grid, xi_stars), theta)),
         bspline_list = attr(x, "bspline_list"),
         call    = match.call(),
         keep_fit = NULL,
         fit    = NULL,
         theta = NULL,
         coefficients = NULL,
         vcov = NULL,
         vcov_theta = NULL,
         loglik = NULL,
         rss    = NULL,
         rse    = NULL)
  class(out) <- c("cpr_cn", class(out))
  out
}

#' @export
#' @rdname cn
#' @param formula a formula that is appropriate for regression method being used.
#' @param data a required \code{data.frame}
#' @param method the regression method such as \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, etc.
#' @param method.args a list of additional arguments to pass to the regression
#' method.
#' @param keep_fit (logical, defaults to \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in the the \code{fit} element.
#' If \code{FALSE} the regression model is not saved and the \code{fit} element
#' will be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if TRUE check that the
#' design matrix is full rank.
cn.formula <- function(formula, data, method = stats::lm, method.args = list(), keep_fit = TRUE, check_rank = TRUE, ...) {

  # check for some formula specification issues
  rhs_check <- grepl("btensor", attr(stats::terms(formula), "term.labels"))
  if ( !rhs_check[1] | any(rhs_check[-1]) ) {
    stop("btensor() must appear first, once, and with no effect modifiers, on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- list(formula = as.name("f_for_use"), data = as.name("data_for_use"))
  cl <- c(cl, method.args)

  fit <- do.call(regression, cl)
  Bmat <- stats::model.frame(fit)[[2]]
  COEF_VCOV <- coef_vcov(fit, theta_idx = seq(1, ncol(Bmat), by = 1))

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m) | any(is.na(COEF_VCOV$coef))) {
      keep_fit <- TRUE
      warning("Design Matrix is rank deficient. keep_fit being set to TRUE.")
    }
  }

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cn")
  cl <- as.call(cl)

  out <- cn.cpr_bt(Bmat, COEF_VCOV$theta)

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
  class(out) <- c("cpr_cn", class(out))

  out
}

#' @export
print.cpr_cn <- function(x, ...) {
  print(x$cn, ...)
  invisible(x)
}


extract_cpr_bsplines <- function(form) {
  B <- NULL
  rr <- function(x) {
    if (is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      B <<- x
    } else if (is.recursive(x)) {
      as.call(lapply(as.list(x), rr))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr)
  B
}
