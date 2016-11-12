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

#' @method plot cpr_cn
#' @export
#' @rdname cn
#' @param show_spline boolean (default FALSE) to plot the spline function within
#' its control net
#' @param color boolean (default FALSE) if more than one \code{cpr_cn} object is
#' to be plotted, set this value to TRUE to have the graphic in color (linetypes
#' will be used regardless of the color settting).
#' @param n the number of data points to use for plotting the spline
#'
plot.cpr_cn <- function(x, ..., show_spline = FALSE, color = FALSE, n = 100) { 
  stop("Needs to be rewritten")
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_spline", "color", "n"))]
  cps   <- lapply(list(x, ...), function(x) x$cn)
  rfctr <- lazyeval::interp( ~ factor(row, levels = seq(1, length(cps)), labels = nms))
  .data <- dplyr::mutate_(dplyr::bind_rows(cps, .id = "row"),
                          .dots = stats::setNames(list(rfctr), "row")) 
                  
  base_plot <- 
    ggplot2::ggplot(.data) +
    ggplot2::theme_bw() + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (length(cps) > 1) { 
    base_plot <- 
      base_plot + 
      ggplot2::aes_string(x = "xi_star", y = "theta", linetype = "factor(row)") + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else { 
    base_plot <- 
      base_plot + ggplot2::aes_string(x = "xi_star", y = "theta")
  }

  if (color) { 
    base_plot <-
      base_plot + 
      ggplot2::aes_string(color = "factor(row)") 
  }
      

  if (show_spline) { 
    .data2 <- 
      lapply(list(x, ...), function(xx) { 
           b <- xx$bknots
           bmat <- cpr::bsplines(seq(b[1], b[2], length = n), 
                                 iknots = xx$iknots, 
                                 bknots = b, 
                                 order  = xx$order)
           data.frame(x = seq(b[1], b[2], length = n), 
                      y = as.numeric(bmat %*% xx$cn$theta))
                          }) 
    .data2 <- 
        dplyr::mutate_(dplyr::bind_rows(.data2, .id = "row"),
                      .dots = stats::setNames(list(rfctr), "row")) 

      base_plot <- 
        base_plot + 
        ggplot2::geom_line(data = .data2, 
                           mapping = ggplot2::aes_string(x = "x", y = "y"))
  }
  base_plot
}

#' @export
#' @param object a \code{cpr_cn} object
#' @rdname cn
summary.cpr_cn <- function(object, ...) {
  out <- 
    c(list(dfs        = length(object$cn$theta),
           loglik     = object$loglik,
           rmse       = object$rmse), 
      setNames(lapply(lapply(object$bspline_list, attr, which = "iknots"), length),
               paste0("n_iknots", seq_along(object$bspline_list))),
      setNames(lapply(lapply(lapply(object$bspline_list, attr, which = "iknots"),
                      function(x) if (length(x)) { x } else { NA }),
                      list),
               paste0("iknots", seq_along(object$bspline_list)))
      )

  dplyr::as_data_frame(out)
}
