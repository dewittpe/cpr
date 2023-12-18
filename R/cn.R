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
#' @examples
#'
#' @export
#' @rdname cn
cn <- function(x, ...) {
  UseMethod("cn")
}

#' @export
#' @rdname cn
#' @param theta a vector of (regression) coefficients, the ordinates of the
#'        control net.
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
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data a required \code{data.frame}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, etc.
#' @param method.args a list of additional arguments to pass to the regression
#' method.
#' @param keep_fit (logical, defaults to \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in the the \code{fit} element.
#' If \code{FALSE} the regression model is not saved and the \code{fit} element will be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if TRUE check that the
#' design matrix is full rank.
cn.formula <- function(formula, data, method = stats::lm, method.args = list(),  keep_fit = TRUE, check_rank = TRUE, ...) {
  # check for some formula specification issues
  fterms <- stats::terms(formula)
  fterms
  if (sum(grepl("btensor", attr(fterms, "term.labels"))) != 1) {
    stop("btensor() must appear once, with no effect modifiers, on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- list(formula = as.name("f_for_use"), data = as.name("data_for_use"))
  cl <- c(cl, method.args)

  fit <- do.call(regression, cl)
  COEF_VCOV <- coef_vcov(fit)

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m) | any(is.na(COEF_VCOV$coef))) {
      if (keep_fit) {
        warning("Design Matrix is rank deficient.")
      } else {
        warning("Design Matrix is rank deficient. keep_fit being set to TRUE.")
        keep_fit <- TRUE
      }
    }
  }

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cn")
  cl <- as.call(cl)

  Bmat <- eval(extract_cpr_bsplines(formula), data, environment(formula))
  #xi_stars <- lapply(attr(Bmat, "bspline_list"), attr, which = "xi_star")

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

#' @method print cpr_cn
#' @export
#' @rdname cn
print.cpr_cn <- function(x, ...) {
  print(x$cn, ...)
}

#' @export
#' @param object a \code{cpr_cn} object
#' @rdname cn
summary.cpr_cn <- function(object, ...) {
  iknots <- lapply(object$bspline_list, attr, which = "iknots")
  names(iknots) <- paste0("iknots", seq_along(iknots))

  out <-
    data.frame(dfs        = length(object$cn$theta),
               loglik     = object$loglik,
               rss        = object$rss,
               rse        = object$rse )

  for(i in seq_along(iknots)) {
    nm <- names(iknots)[i]
    out[[paste0("n_", nm)]] <- length(iknots[[i]])
    out[[nm]] <- I(iknots[i])
  }

  out

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
