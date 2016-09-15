#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' \code{cpr} runs the control polygon reduction algorithm
#'
#' @param x a \code{cpr_bs} object 
#' @param ... arguments passed to the regression method
#'
#' @examples
#' 
#' # Support
#' xvec <- seq(0, 6, length = 500)
#' 
#' # Define the basis matrix
#' bmat1 <- cpr::bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5))
#' bmat2 <- cpr::bsplines(x = xvec)
#' 
#' # Define the control vertice ordinates
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
#' dat <- dplyr::data_frame(x = xvec, y = sin((x - 2)/pi) + 1.4 * cos(x/pi))
#' cp3 <- cp(y ~ cpr::bsplines(x) + 0, data = dat)
#' 
#' # plot the control polygon, spline and traget data.
#' plot(cp3, show_spline = TRUE) + 
#'   ggplot2::geom_line(mapping = ggplot2::aes_string(x = "x", y = "y"), 
#'                      data = dat, linetype = 2, color = "red")
#' 
#' @export
#' @rdname cp
cp <- function(x, ...) { 
  UseMethod("cp")
}

#' @export
#' @rdname cp
#' @param theta a vector of (regression) coefficients, the ordinates of the
#'        control polygon.
cp.cpr_bs <- function(x, theta, ...) { 
  out <- list(cp = dplyr::data_frame(xi_star = as.numeric(attr(x, "xi_star")), 
                                     theta   = as.vector(theta)),
              xi = c(attr(x, "xi")),
              iknots = c(attr(x, "iknots")),
              bknots = c(attr(x, "bknots")),
              order  = attr(x, "order"),
              call   = match.call(),
              fit    = NA,
              loglik = NA,
              rmse   = NA,
              wiggle = NA) 
  class(out) <- c("cpr_cp", class(out))
  out$wiggle <- wiggle(out)
  out
}

#' @export
#' @rdname cp
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
cp.formula <- function(formula, data = parent.frame(), method = stats::lm, ...) { 
  # check for some formula specification issues
  fterms <- stats::terms(formula)
  fterms
  if (sum(grepl("bsplines", attr(fterms, "term.labels"))) != 1) {
    stop("cpr::bsplines() must apear once, with no effect modifiers, on the right hand side of the formula.")
  }
   
  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method")))]
  cl$formula <- f_for_use
  cl$data <- data_for_use

  fit <- do.call(regression, cl)

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cp")
  cl <- as.call(cl)

  Bmat <- eval(extract_cpr_bsplines(formula), data, environment(formula))
  out <- cp.cpr_bs(Bmat, as.vector(theta(fit)))

  out$call    = cl
  out$fit     = fit
  out$loglik  = loglikelihood(fit)
  out$rmse    = sqrt(mean(stats::residuals(fit)^2))
  out$wiggle  = wiggle(out)
                       
  class(out) <- c("cpr_cp", class(out))

  out
}

#' @method print cpr_cp
#' @export
#' @rdname cp
print.cpr_cp <- function(x, ...) { 
  print(x$cp, ...)
}

#' @method plot cpr_cp
#' @export
#' @rdname cp
#' @param show_spline boolean (default FALSE) to plot the spline function within
#' its control polygon
#' @param color boolean (default FALSE) if more than one \code{cpr_cp} object is
#' to be plotted, set this value to TRUE to have the graphic in color (linetypes
#' will be used regardless of the color settting).
#' @param n the number of data points to use for plotting the spline
#'
plot.cpr_cp <- function(x, ..., show_spline = FALSE, color = FALSE, n = 100) { 
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_spline", "color", "n"))]
  cps   <- lapply(list(x, ...), function(x) x$cp)
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
                      y = as.numeric(bmat %*% xx$cp$theta))
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

