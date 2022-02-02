#' Control Nets
#'
#' Generate the control net for a uni-variable B-spline
#'
#' \code{cn} generates the control net for the given B-spline function.  There
#' are several methods for building a control net.
#'
#' @param x a \code{cpr_bs} object
#' @param ... arguments passed to the regression method
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
#'  \item{rmse}{The root mean squared error for the regression models}
#'  }
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
         keep_fit = NA,
         fit     = NA,
         loglik  = NA,
         rmse    = NA)
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
#' @param keep_fit (logical, defaults to \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in the the \code{fit} element.
#' If \code{FALSE} the regression model is not saved and the \code{fit} element will be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if TRUE check that the
#' design matrix is full rank.
cn.formula <- function(formula, data, method = stats::lm, ..., keep_fit = FALSE, check_rank = TRUE) {
  # check for some formula specification issues
  fterms <- stats::terms(formula)
  fterms
  if (sum(grepl("btensor", attr(fterms, "term.labels"))) != 1) {
    stop("cpr::btensor() must appear once, with no effect modifiers, on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit", "check_rank")))]
  cl$formula <- as.name("f_for_use")
  cl$data <- as.name("data_for_use")

  fit <- do.call(regression, cl)

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m) | any(is.na(BETA(fit)))) {
      warning("Design Matrix is rank deficient. keep_fit being set to TRUE.",
              call. = FALSE,
              immediate. = TRUE)
    keep_fit <- TRUE
    }
  }

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cn")
  cl <- as.call(cl)

  Bmat <- eval(extract_cpr_bsplines(formula), data, environment(formula))
  xi_stars <- lapply(attr(Bmat, "bspline_list"), attr, which = "xi_star")

  out <-
    list(cn      = data.frame(cbind(do.call(expand.grid, xi_stars),
                                 theta   = as.vector(theta(fit)))),
         bspline_list = attr(Bmat, "bspline_list"),
         call    = cl,
         keep_fit = keep_fit,
         fit     = if (keep_fit) { fit } else { NA },
         coefficients = BETA(fit),
         vcov = SIGMA(fit),
         loglik  = loglikelihood(fit),
         rmse    = sqrt(mean(stats::residuals(fit)^2)))
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
               rmse       = object$rmse)

  for(i in seq_along(iknots)) {
    nm <- names(iknots)[i]
    out[[paste0("n_", nm)]] <- length(iknots[[i]])
    out[[nm]] <- I(iknots[i])
  }

  out

}
