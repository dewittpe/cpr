#' Control Nets
#'
#' Generate the control net for a uni-variable B-spline
#'
#' \code{cn} generates the control net for the given B-spline function.  
#'
#' \code{cpr} runs the control net reduction algorithm
#'
#' @param x a \code{cpr_bs} object 
#' @param ... arguments passed to the regression method
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
    list(cn      = dplyr::tbl_df(cbind(do.call(expand.grid, xi_stars), theta)),
         # xi      = attr(x, "xi"),
         # xi_star = attr(x, "xi_star"),
         # theta   = theta,
         # iknots  = attr(x, "iknots"),
         # bknots  = attr(x, "bknots"),
         # order   = attr(x, "order"), 
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
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param keep_fit (logical, defaults to \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in the the \code{fit} element.
#' If \code{FALSE} the regression model is not saved and the \code{fit} element will be \code{NA}.
cn.formula <- function(formula, data = parent.frame(), method = stats::lm, ..., keep_fit = FALSE) { 
  # check for some formula specification issues
  fterms <- stats::terms(formula)
  fterms
  if (sum(grepl("btensor", attr(fterms, "term.labels"))) != 1) {
    stop("cpr::btensor() must apear once, with no effect modifiers, on the right hand side of the formula.")
  }
   
  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit")))]
  cl$formula <- f_for_use
  cl$data <- data_for_use

  fit <- do.call(regression, cl)

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cn")
  cl <- as.call(cl)

  Bmat <- eval(extract_cpr_bsplines(formula), data, environment(formula))
  xi_stars <- lapply(attr(Bmat, "bspline_list"), attr, which = "xi_star")

  out <-
    list(cn      = dplyr::tbl_df(cbind(do.call(expand.grid, xi_stars),
                                 theta   = as.vector(theta(fit)))), 
         bspline_list = attr(Bmat, "bspline_list"),
         call    = cl,
         keep_fit = keep_fit,
         fit     = if (keep_fit) { fit } else { NA },
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

#' Plotting Control Nets
#'
#' One- and Two-dimensional plots of control nets.
#'
#' @method plot cpr_cn
#' @export
#' @param x a \code{cpr_cn} object
#' @param margin an integer vector or length 1 or 2.  For length 1, the marginal
#' control polygon is plotted via a call to \code{\link{plot.cpr_cp}}.  For
#' length 2, the (marginal) tensor surface is plotted.  See details.
#' @param at point value for marginals not defined in the \code{margin}.  See
#' details.
#' @param \ldots arguments passed to \code{\link{plot.cpr_cp}}
#'
plot.cpr_cn <- function(x, margin = 1:2, at, ...) { 
  if (missing(at)) { 
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  } 

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  if (length(margin) == 1L) {
    mbs <- mapply(bsplines, x = at, iknots = iknots, bknots = bknots, order = orders, SIMPLIFY = FALSE)
    tensor <- build_tensor(mbs[-margin])
    thetas <- apply(array(x$cn$theta, dim = dfs), margin, function(x) x)
    marginal_cp <- cp(x$bspline_list[[margin]], t(tensor %*% thetas))

    plot(marginal_cp, ...) 

  } else if (length(margin) == 2L) {


    if (length(x$bspline_list) == 2L) {
      xvecs <- do.call(expand.grid, lapply(bknots, function(x) seq(x[1], x[2], length = 100)))
      tp  <- btensor(xvecs, iknots = iknots, bknots = bknots, order = orders) 
      dat <- data.frame(x = xvecs[[1]], y = xvecs[[2]], z = tp %*% x$cn$theta)
      ggplot2::ggplot(dat) +
      ggplot2::aes_string(x = "x", y = "y", fill = "z") +
      ggplot2::geom_tile()
    } else {
      stop("not yet implimented for more than two 2D")
    } 

  } else {
    stop("margin needs to be a length one or two integer vector")
  }
}

#' @export
#' @param object a \code{cpr_cn} object
#' @rdname cn
summary.cpr_cn <- function(object, ...) {
  out <- 
    c(list(dfs        = length(object$cn$theta),
           loglik     = object$loglik,
           rmse       = object$rmse), 
      stats::setNames(lapply(lapply(object$bspline_list, attr, which = "iknots"), length),
                      paste0("n_iknots", seq_along(object$bspline_list))),
      stats::setNames(lapply(lapply(lapply(object$bspline_list, attr, which = "iknots"),
                                    function(x) if (length(x)) { x } else { NA }),
                             list),
                      paste0("iknots", seq_along(object$bspline_list)))
      )

  dplyr::as_data_frame(out)
}
