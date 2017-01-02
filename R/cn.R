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
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
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
    stop("cpr::btensor() must apear once, with no effect modifiers, on the right hand side of the formula.")
  }
   
  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m)) warning("Design Matrix is rank deficient. keep_fit being set to TRUE.",
                                           call. = FALSE,
                                           immediate. = TRUE)
    keep_fit <- TRUE
  } 

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit", "check_rank")))]
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

#' Plotting Control Nets
#'
#' One- and Two-dimensional plots of control nets.
#'
#' @method plot cpr_cn
#' @export
#' @param x a \code{cpr_cn} object
#' @param \ldots ignored
#' @param margin an integer vector or length 1 or 2.  For length 1, the marginal
#' control polygon is plotted via a call to \code{\link{plot.cpr_cp}}.  For
#' length 2, the (marginal) tensor surface is plotted.  See details.
#' @param at point value for marginals not defined in the \code{margin}.  See
#' details.
#' @param show_net logical, show the control net
#' @param show_surface logical, show the tensor product surface
#' @param xlim,ylim,zlim x-, y- and z-limits.  If present, the plot is clipped to this region.
#' @param xlab,ylab,zlab titles for the axes.  N.B. These must be character
#' strings; expressions are not accepted.  Numbers will be coerced to character
#' strings.
#' @param aspect aspect ratio of the graphic
#' @param net_args \code{\link[rgl]{rgl.material}} and other arguments passed to
#' \code{\link[rgl]{persp3d}} to modify the appearance of the control net.
#' Defaults to \code{front = "lines", back = "lines"}.
#' @param surface_args \code{\link[rgl]{rgl.material}} and other arguments
#' passed to \code{\link[rgl]{persp3d}} to modify the appearance of the surface.
#' Defaults to \code{front = "lines", back = "lines"}.  
#' @param cp_args arguments passed to \code{\link{plot.cpr_cp}}, only used if
#' \code{length(margin) == 1L}.
#'
#' @seealso \code{\link{plot.cpr_cp}}, \code{\link[rgl]{persp3d}},
#' \code{\link[rgl]{rgl.material}}
#'
plot.cpr_cn <- function(x, ..., margin = 1:2, at,
                        show_net = TRUE, show_surface = FALSE,
                        xlim, ylim, zlim,
                        xlab = "", ylab = "", zlab = "", 
                        aspect,
                        net_args = list(col = "black"),
                        surface_args = list(col = "grey20"),
                        cp_args = list(show_cp = TRUE, show_spline = TRUE, show_xi = FALSE)) { 
  if (missing(at)) { 
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  } 
  
  if (!is.list(at) | length(at) != length(x$bspline_list)) {
    stop("the `at` argument needs to be a list of the same length as `x$bspline_list`.")
  } 

  net_args$xlab <- surface_args$xlab <- xlab 
  net_args$ylab <- surface_args$ylab <- ylab 
  net_args$zlab <- surface_args$zlab <- zlab

  if (!missing(xlim)) {
    net_args$xlim <- surface_args$xlim <- xlim
  }

  if (!missing(ylim)) {
    net_args$ylim <- surface_args$ylim <- ylim
  }
   
  if (!missing(zlim)) {
    net_args$zlim <- surface_args$zlim <- zlim
  }

  if (!missing(aspect)) {
    net_args$aspect <- surface_args$aspect <- aspect
  }

  if (is.null(surface_args$front)) {
    surface_args$front <- 'fill'
  }

  if (is.null(surface_args$back)) {
    surface_args$back <- 'lines'
  }

  if (is.null(net_args$front)) {
    net_args$front <- 'lines'
  }

  if (is.null(net_args$back)) {
    net_args$back <- 'lines'
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

    eval(do.call(call, c(list(name = "plot", x = marginal_cp), cp_args)), environment())

  } else if (length(margin) == 2L) {

    if (show_net) {
      xvecs <- lapply(x$bspline_list, attr, which = "xi_star")
      xvecs[-margin] <- at[-margin] 
      net <- do.call(expand.grid, xvecs) 
      tensors <- Map(btensor,
                     x = split(net, row(net)[, 1]), 
                     MoreArgs = list(iknots = iknots,
                                     bknots = bknots,
                                     order = orders)) 
      net$z <- do.call(c, Map(`%*%`, x = tensors,
                                  MoreArgs = list(y = x$cn$theta))) 
    }

    if (show_surface) { 
      xvecs <- lapply(bknots, function(x) seq(x[1], x[2], length = 100))
      xvecs[-margin] <- at[-margin] 
      surface <- do.call(expand.grid, xvecs) 
      tensors <- Map(btensor,
                     x = split(surface, row(surface)[, 1]), 
                     MoreArgs = list(iknots = iknots,
                                     bknots = bknots,
                                     order = orders)) 
      surface$z <- do.call(c, Map(`%*%`, x = tensors,
                                  MoreArgs = list(y = x$cn$theta)))
    }

    if (show_net) { 
        do.call(rgl::persp3d,
                c(
                  list(x = unique(net[[margin[1]]]),
                       y = unique(net[[margin[2]]]),
                       z = matrix(net[['z']],
                                  nrow = dplyr::n_distinct(net[[margin[1]]]),
                                  ncol = dplyr::n_distinct(net[[margin[2]]]))),
                  net_args)
                )
      if (show_surface) {
        do.call(rgl::persp3d,
                c(
                  list(x = unique(surface[[margin[1]]]),
                       y = unique(surface[[margin[2]]]),
                       z = matrix(surface[['z']],
                                  nrow = dplyr::n_distinct(surface[[margin[1]]]),
                                  ncol = dplyr::n_distinct(surface[[margin[2]]])),
                       add = TRUE),
                  surface_args)
                )
      }

    } else if (!show_net & show_surface) {
        do.call(rgl::persp3d,
                c(
                  list(x = unique(surface[[margin[1]]]),
                       y = unique(surface[[margin[2]]]),
                       z = matrix(surface[['z']],
                                  nrow = dplyr::n_distinct(surface[[margin[1]]]),
                                  ncol = dplyr::n_distinct(surface[[margin[2]]]))),
                  surface_args)
                )
    } else {
      warning("Nothing to plot.")
      return(invisible())
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
